
function [diamFail,diamInit,runTime,cpuTime] = calcDiamPower_V3(A,failures,failType,compFromNode,compToNode)

sizeFailures = size(failures);
numFailures = sizeFailures(1);          % number of failure scenarios
numFailElements = sizeFailures(2);      % number of total elements to fail in each scenario

n = length(A);

for i = 1:numFailures                   % for each failure simulation
    
    Afailure = A;
    
    runTimeStart = tic;
    cpuTimeStart = cputime;  
                
    for j = 1:numFailElements
        switch failType
            case 'N'                    % for node failures
                Afailure(failures(i,j),:) = 0;
                Afailure(:,failures(i,j)) = 0;
                dFailure = zeros(n,n);
                
                for s = 1:n
                    dFailureTmp = dijkstraV3power(Afailure,s,n);
                    dFailure(s,:) = dFailureTmp;
                end
                diamFail(i,j) = max(dFailure(dFailure~=Inf));
                    
            case 'E'
                for k = 1:n
                    if (Afailure(compFromNode(failures(i,j)),compToNode(failures(i,j)))) ~= 0
                    	Afailure(compFromNode(failures(i,j)),compToNode(failures(i,j))) = Afailure(compFromNode(failures(i,j)),compToNode(failures(i,j))) - 1;
                    end
                    if (Afailure(compToNode(failures(i,j)),compFromNode(failures(i,j)))) ~= 0
                        Afailure(compToNode(failures(i,j)),compFromNode(failures(i,j))) = Afailure(compToNode(failures(i,j)),compFromNode(failures(i,j))) - 1;
                    end
                end
                
                dFailure = zeros(n,n);

                for s = 1:n
                    dFailureTmp = dijkstraV3power(Afailure,s,n);
                    dFailure(s,:) = dFailureTmp;
                end
                diamFail(i,j) = max(dFailure(dFailure~=Inf));
        end
    end
    
        
    runTime(i) = toc(runTimeStart);
    cpuTime(i) = cputime - cpuTimeStart;
    i
    
end

dFailure = zeros(n,n);
% Calculate initial efficiency
for s = 1:n
	dFailureTmp = dijkstraV3power(A,s,n);
	dFailure(s,:) = dFailureTmp;
end
dFailureInv = 1./dFailure;
diamInit = max(dFailure(dFailure~=Inf));