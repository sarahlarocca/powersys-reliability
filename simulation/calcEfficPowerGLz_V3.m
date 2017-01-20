
function [EfailGLz,EinitGLz,runTime,cpuTime] = calcEfficPowerGLz_V3(A,failures,failType,compFromNode,compToNode,genData,nodeList,branchData)

sizeFailures = size(failures);
numFailures = sizeFailures(1);          % number of failure scenarios
numFailElements = sizeFailures(2);      % number of total elements to fail in each scenario

n = length(A);

EfailGLz = zeros(numFailures,numFailElements);       % initial matrix of efficiencyGL

genList = [];   % initialize list of generators
subList = [];   % initialize list of substations

for i = 1:size(genData)
    if genData(i,2) > 0
        if ~(any(genList == genData(i,1)))
            genList = [genList genData(i,1)];
        end
    elseif genData(i,2) < 0
        if ~(any(subList == genData(i,1)))
            subList = [subList genData(i,1)];
        end
    end
end

numGen = length(genList);
numSub = length(subList);

for i = 1:numGen
    genList(i) = find(nodeList == genList(i));
end

for i = 1:numSub
    subList(i) = find(nodeList == subList(i));
end

for i = 1:numFailures                   % for each failure simulation
    Afailure = A;
    
    runTimeStart = tic;
    cpuTimeStart = cputime;      
    
    for j = 1:numFailElements
        switch failType
            case 'N'                    % for node failures
                Afailure(failures(i,j),:) = 0;
                Afailure(:,failures(i,j)) = 0;
                dFailure = [];
                for k = 1:numSub
                    dFailureTmp = dijkstraV3powerZ(Afailure,subList(k),n,branchData,nodeList);
                    for q = 1:n
                        if (q ~= subList(k)) && any(genList == q)
                            dFailure = [dFailure dFailureTmp(q)];
                        end
                    end
                end
                                     
                dFailureInv = 1./dFailure;
                EfailGLz(i,j) = mean(dFailureInv);
                    
            case 'E'
                for k = 1:n
                    if (Afailure(compFromNode(failures(i,j)),compToNode(failures(i,j)))) ~= 0
                    	Afailure(compFromNode(failures(i,j)),compToNode(failures(i,j))) = Afailure(compFromNode(failures(i,j)),compToNode(failures(i,j))) - 1;
                    end
                    if (Afailure(compToNode(failures(i,j)),compFromNode(failures(i,j)))) ~= 0
                        Afailure(compToNode(failures(i,j)),compFromNode(failures(i,j))) = Afailure(compToNode(failures(i,j)),compFromNode(failures(i,j))) - 1;
                    end
                end
                
                dFailure = [];
                for k = 1:numSub
                    dFailureTmp = dijkstraV3powerZ(Afailure,subList(k),n,branchData,nodeList);
                    for q = 1:n
                        if (q ~= subList(k)) && any(genList == q)
                            dFailure = [dFailure dFailureTmp(q)];
                        end
                    end
                end
                                     
                dFailureInv = 1./dFailure;
                EfailGLz(i,j) = mean(dFailureInv);
        end
    end
    
    runTime(i) = toc(runTimeStart);
    cpuTime(i) = cputime - cpuTimeStart;
    i
end

dFailure = [];
% Calculate initial efficiency
for k = 1:numSub
    dFailureTmp = dijkstraV3powerZ(A,subList(k),n,branchData,nodeList);
    for q = 1:n
        if (q ~= subList(k)) && any(genList == q)
            dFailure = [dFailure dFailureTmp(q)];
        end
    end
end
dFailureInv = 1./dFailure;
EinitGLz = mean(dFailureInv(dFailureInv~=Inf));