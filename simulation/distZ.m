function [dZ] = distZ(nodeA,nodeB,branchData,nodeList)

nodeAnum = nodeList(nodeA);
nodeBnum = nodeList(nodeB);

numEdges = length(branchData);

for i = 1:numEdges
	if (((branchData(i,1) == nodeAnum) && (branchData(i,2) == nodeBnum)) || ((branchData(i,1) == nodeBnum) && (branchData(i,2) == nodeAnum)))
        dZ = sqrt(((branchData(i,3))^2) + ((branchData(i,4))^2));
    end
end
