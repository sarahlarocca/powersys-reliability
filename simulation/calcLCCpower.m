
function [sizeSfail] = calcLCCpower(A,failures,failType,compFromNode,compToNode)

sizeFailures = size(failures);
numFailures = sizeFailures(1);          % number of failure scenarios
numFailElements = sizeFailures(2);      % number of total elements to fail in each scenario

n = length(A);

sizeSfail = zeros(numFailures,numFailElements); % initial matrix of largest connected component size

for i = 1:numFailures                   % for each failure simulation
    Afailure = A;
    for j = 1:numFailElements
        switch failType
            case 'N'                    % for node failures
                for k = 1:n
                    if (Afailure(failures(i,j),k) ~= 0)
                        Afailure(failures(i,j),k) = Afailure(failures(i,j),k) - 1;
                    end
                    if (Afailure(k,failures(i,j)) ~= 0)
                        Afailure(k,failures(i,j)) = Afailure(k,failures(i,j)) - 1;
                    end
                end
                [~,sizeSfail(i,j),~] = connectedComponents(Afailure,n);
            case 'E'
                Afailure(compFromNode(failures(i,j)),compToNode(failures(i,j))) = Afailure(compFromNode(failures(i,j)),compToNode(failures(i,j))) - 1;
                Afailure(compToNode(failures(i,j)),compFromNode(failures(i,j))) = Afailure(compToNode(failures(i,j)),compFromNode(failures(i,j))) - 1;
                [~,sizeSfail(i,j),~] = connectedComponents(Afailure,n);
        end
    end
    i
end