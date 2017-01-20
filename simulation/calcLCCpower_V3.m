
function [sizeSfail, runTime, cpuTime] = calcLCCpower_V3(A,failures,failType,compFromNode,compToNode)

sizeFailures = size(failures);
numFailures = sizeFailures(1);          % number of failure scenarios
numFailElements = sizeFailures(2);      % number of total elements to fail in each scenario

n = length(A);

sizeSfail = zeros(numFailures,numFailElements); % initial matrix of largest connected component size

for i = 1:numFailures                   % for each failure simulation
    Afailure = A;
    runTimeStart = tic;
    cpuTimeStart = cputime;   
    for j = 1:numFailElements
        switch failTypeStrain
            case 'N'                    % for node failures
                Afailure(failures(i,j),:) = 0;
                Afailure(:,failures(i,j)) = 0;
                [~,sizeSfail(i,j),~] = connectedComponents(Afailure,n);
            case 'E'
                Afailure(compFromNode(failures(i,j)),compToNode(failures(i,j))) = Afailure(compFromNode(failures(i,j)),compToNode(failures(i,j))) - 1;
                Afailure(compToNode(failures(i,j)),compFromNode(failures(i,j))) = Afailure(compToNode(failures(i,j)),compFromNode(failures(i,j))) - 1;
                [~,sizeSfail(i,j),~] = connectedComponents(Afailure,n);
        end
    end
    
    runTime(i) = toc(runTimeStart);
    cpuTime(i) = cputime - cpuTimeStart;
    i
end