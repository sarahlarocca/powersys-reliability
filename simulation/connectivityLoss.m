function [connectLoss] = connectivityLoss(A,failures,failType,compFromNode,compToNode,genData,nodeList)

sizeFailures = size(failures);
numFailures = sizeFailures(1);          % number of failure scenarios
numFailElements = sizeFailures(2);      % number of total elements to fail in each scenario

n = length(A);

connectLoss = zeros(numFailures,numFailElements); % initial matrix of connectivity loss

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
    for j = 1:numFailElements
        switch failType
            case 'N'                    % for node failures
                Afailure(failures(i,j),:) = 0;
                Afailure(:,failures(i,j)) = 0;
                numConnectGen = zeros(1,numSub);
                for k = 1:numSub                        % for all distribution substations
                    [d] = dijkstraV3power(Afailure,subList(k),n);
                    for q = 1:numGen                       % for all generators
                        if (genList(q) == subList(k)) && (sum(Afailure(subList(k),:))==0)
                        elseif (d(genList(q)) < Inf)
                            numConnectGen(k) = numConnectGen(k) + 1;
                        end
                    end
                end
                connectLoss(i,j) = 1 - ((sum(numConnectGen./numGen))/numSub);
            case 'E'
                for p = 1:n
                    if (Afailure(compFromNode(failures(i,j)),compToNode(failures(i,j)))) ~= 0
                    	Afailure(compFromNode(failures(i,j)),compToNode(failures(i,j))) = Afailure(compFromNode(failures(i,j)),compToNode(failures(i,j))) - 1;
                    end
                    if (Afailure(compToNode(failures(i,j)),compFromNode(failures(i,j)))) ~= 0
                        Afailure(compToNode(failures(i,j)),compFromNode(failures(i,j))) = Afailure(compToNode(failures(i,j)),compFromNode(failures(i,j))) - 1;
                    end
                end
                numConnectGen = zeros(1,numSub);
                for k = 1:numSub                        % for all distribution substations
                    [d] = dijkstraV3power(Afailure,subList(k),n);
                    for q = 1:numGen                       % for all generators
                        if (d(genList(q)) < Inf)
                            numConnectGen(k) = numConnectGen(k) + 1;
                        end
                    end
                end
                connectLoss(i,j) = 1 - ((sum(numConnectGen./numGen))/numSub);
        end
    end
    i
end
    