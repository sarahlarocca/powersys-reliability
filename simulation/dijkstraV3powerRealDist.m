function [d] = dijkstraV3powerRealDist(A,s,n)

% A = adjacency matrix
% s = source
% n = number of nodes in graph

d = inf(1,n);
p = zeros(1,n);

d(s) = 0;

S = [];
Q = 1:n;

while ~isempty(Q)
    u = find(d==min(d(Q)));
    S = [S u];
    for i = 1:length(u)
        Q = Q(Q~=u(i));
        uAdj = find((A(u(i),:)~=0));
        uAdj = uAdj(uAdj~=u(i));
        for v = uAdj
            if d(v) > d(u(i)) + distReal(u(i),v)
                d(v) = d(u(i)) + distReal(u(i),v);
                p(v) = u(i);
            end
        end
    end
end