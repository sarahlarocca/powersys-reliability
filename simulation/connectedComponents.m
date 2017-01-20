function [S,sizeS,disconnectedComponents] = connectedComponents(A,n)

S = [];                                 % initialize largest connected component
sizeS = 0;                              % initialize size of largest connected component
disconnectedComponents = cell(1,1);     % initialize cell of disconnected component matrices

unexplored = 1:n;                       % list of unexplored nodes

while ~isempty(unexplored)     % while not all nodes have been found in a component
    visited = zeros(1,n);           % initialize vector of visited nodes; 0 = not visited, 1 = visited
    Q = [];                         % initialize queue

    v = unexplored(1);              % select start node from list of unexplored nodes

    visited(v) = 1;                 % mark start node as visited
    Q = v;                          % add start node to queue

    while ~isempty(Q);
        u = Q(1);   
        Wu = find(A(u,:));          % find neighbors of u
        Wu = Wu(Wu ~= u);
        for w = Wu
            if (visited(w) == 0)
                Q = [Q w];
                visited(w) = 1;
            end
        end
        Q = Q(2:length(Q));
    end

    componentNodes = [];                        
    for i = 1:n
        if visited(i) == 1
            componentNodes = [componentNodes i];        % make list of nodes in current component
        end
    end

    sizeComponent = sum(visited);                       % size of current component
    if sizeComponent > sizeS                            % if current component is larger that largest component
        if ~isempty(S)
            Sprev = S;                                      % set Sprev to S
            disconnectedComponents{(length(disconnectedComponents)+1)} = Sprev;
        end
        S = componentNodes;                             % assign current component as largest component
        sizeS = sizeComponent;
    else
        disconnectedComponents{(length(disconnectedComponents)+1)} = componentNodes;      % add component to list of disconnected components
    end

    unexplored = setdiff(unexplored,componentNodes);         % update list of unexplored nodes
end