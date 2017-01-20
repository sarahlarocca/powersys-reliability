% This script generates a Matpower case file incorporating
% negative generation for use with load shedding.




%% Initial data

initCase = case2736sp;		% load data from initial Matpower case file

%% Case format

mpc.version = '2';          % current Matpower file format

%% ----- POWER FLOW DATA -----

%% System MVA base

mpc.baseMVA = 100;

%% Bus data

numBus = length(initCase.bus);              % number of buses in the system
mpc.bus = [];                               % initialize bus data matrix

for i = 1:numBus
    mpc.bus(i,1) = initCase.bus(i,1);       % bus number (positive integer)
	mpc.bus(i,2) = initCase.bus(i,2);       % bus type (1 = PQ bus; 2 = PV bus; 3 = reference bus; 4 = isolated bus)
	mpc.bus(i,3) = 0;                       % Pd, real power demand (MW)
	mpc.bus(i,4) = 0;                       % Qd, reactive power demand (MVAr)
	mpc.bus(i,5) = initCase.bus(i,5);       % Gs, shunt conductance (MW demanded at V = 1.0 p.u.)
	mpc.bus(i,6) = initCase.bus(i,6);       % Bs, shunt susceptance (MVAr injected at V = 1.0 p.u.)
	mpc.bus(i,7) = initCase.bus(i,7);       % area number (positive integer)
	mpc.bus(i,8) = initCase.bus(i,8);       % Vm, voltage magnitude (p.u.)
	mpc.bus(i,9) = initCase.bus(i,9);       % Va, voltage angle (degree)
	mpc.bus(i,10) = initCase.bus(i,10);     % baseKV, base voltage (kV)
	mpc.bus(i,11) = initCase.bus(i,11);     % zone, loss zone (positive integer)
	mpc.bus(i,12) = initCase.bus(i,12);     % maxVm, maximum voltage magnitude (p.u.)
	mpc.bus(i,13) = initCase.bus(i,13);     % minVm, minimum voltage magnitude (p.u.)
end


%% Generator data

numGen = length(initCase.gen);              % number of generators in the system
mpc.gen = [];                               % initialize generator data matrix

for i = 1:numGen
	mpc.gen(i,1) = initCase.gen(i,1);       % bus number
	mpc.gen(i,2) = initCase.gen(i,2);       % Pg, real power output (MW)
	mpc.gen(i,3) = initCase.gen(i,3);       % Qg, reactive power output (MVAr)
	mpc.gen(i,4) = initCase.gen(i,4);       % Qmax, maximum reactive power output (MVAr)
	mpc.gen(i,5) = initCase.gen(i,5);       % Qmin, minimum reactive power output (MVAr)
	mpc.gen(i,6) = initCase.gen(i,6);       % Vg, voltage magnitude setpoint (p.u.)
	mpc.gen(i,7) = initCase.gen(i,7);       % mBase, total MVA base of machine (defaults to baseMVA)
	mpc.gen(i,8) = initCase.gen(i,8);       % status (>0 := machine in service; <=0 := machine out of service)
	mpc.gen(i,9) = initCase.gen(i,9);       % Pmax, maximum real power output (MW)
	mpc.gen(i,10) = initCase.gen(i,10);     % Pmin, minimum real power output (MW)
	mpc.gen(i,11) = initCase.gen(i,11);     % Pc1, lower real power output of PQ capability curve (MW)
	mpc.gen(i,12) = initCase.gen(i,12);     % Pc2, upper real power output of PQ capability curve (MW)
	mpc.gen(i,13) = initCase.gen(i,13);     % Qc1min, minimum reactive power output at Pc1 (MVAr)
	mpc.gen(i,14) = initCase.gen(i,14);     % Qc1max, maximum reactive power output at Pc1 (MVAr)
	mpc.gen(i,15) = initCase.gen(i,15);     % Qc2min, minimum reactive power output at Pc2 (MVAr)
	mpc.gen(i,16) = initCase.gen(i,16);     % Qc2max, maximum reactive power output at Pc2 (MVAr)
	mpc.gen(i,17) = initCase.gen(i,17);     % rampAGC, ramp rate for load following/AGC (MW/min)
	mpc.gen(i,18) = initCase.gen(i,18);     % ramp10, ramp rate for 10 minute reserves (MW)
	mpc.gen(i,19) = initCase.gen(i,19);     % ramp30, ramp rate for 30 minute reserves (MW)
	mpc.gen(i,20) = initCase.gen(i,20);     % rampQ, ramp rate for reactive power (2 sec timescale) (MVAr/min)
	mpc.gen(i,20) = initCase.gen(i,21);     % APF, area participation factor
end

numLoad = length(find((initCase.bus(:,3)~=0)|(initCase.bus(:,4)~=0)));      % number of load buses
listLoad = find((initCase.bus(:,3)~=0)|(initCase.bus(:,4)~=0));             % list of load buses 

for i = (numGen + 1):(numGen + numLoad)
	mpc.gen(i,1) = initCase.bus(listLoad(i-numGen),1);     % bus number
	mpc.gen(i,2) = -initCase.bus(listLoad(i-numGen),3);    % Pg, real power output (MW)
	mpc.gen(i,3) = -initCase.bus(listLoad(i-numGen),4);    % Qg, reactive power output (MVAr)
	mpc.gen(i,4) = 0;                               % Qmax, maximum reactive power output (MVAr)
	mpc.gen(i,5) = -initCase.bus(listLoad(i-numGen),4);    % Qmin, minimum reactive power output (MVAr)
    
    if ~isempty(find(initCase.gen(:,1) == initCase.bus(listLoad(i-numGen),1)))                                     % if current bus is a generator, set Vg to setpoint from above
        mpc.gen(i,6) = max(initCase.gen(find(initCase.gen(:,1)==initCase.bus(listLoad(i-numGen),1)),6));              % Vg, voltage magnitude setpoint (p.u.)            
    else
        mpc.gen(i,6) = 0.98;                                                                              % (arbitrary - can change this)
    end

	if ~isempty(find(initCase.gen(:,1) == initCase.bus(listLoad(i-numGen),1)))                                     % if current bus is a generator, set mBase to MVA base from above
        mpc.gen(i,7) = max(initCase.gen(find(initCase.gen(:,1)==initCase.bus(listLoad(i-numGen),1)),7));              % mBase, total MVA base of machine (defaults to baseMVA)           
    else
        mpc.gen(i,7) = mpc.baseMVA;                                                                       % (default - can change this)
    end   
    
	mpc.gen(i,8) = 1;                               % status (>0 := machine in service; <=0 := machine out of service)
	mpc.gen(i,9) = 0;                               % Pmax, maximum real power output (MW)
	mpc.gen(i,10) = -initCase.bus(listLoad(i-numGen),3);   % Pmin, minimum real power output (MW)
	mpc.gen(i,11) = 0;                              % Pc1, lower real power output of PQ capability curve (MW)
	mpc.gen(i,12) = 0;                              % Pc2, upper real power output of PQ capability curve (MW)
	mpc.gen(i,13) = 0;                              % Qc1min, minimum reactive power output at Pc1 (MVAr)
	mpc.gen(i,14) = 0;                              % Qc1max, maximum reactive power output at Pc1 (MVAr)
	mpc.gen(i,15) = 0;                              % Qc2min, minimum reactive power output at Pc2 (MVAr)
	mpc.gen(i,16) = 0;                              % Qc2max, maximum reactive power output at Pc2 (MVAr)
	mpc.gen(i,17) = 0;                              % rampAGC, ramp rate for load following/AGC (MW/min)
	mpc.gen(i,18) = 0;                              % ramp10, ramp rate for 10 minute reserves (MW)
	mpc.gen(i,19) = 0;                              % ramp30, ramp rate for 30 minute reserves (MW)
	mpc.gen(i,20) = 0;                              % rampQ, ramp rate for reactive power (2 sec timescale) (MVAr/min)
	mpc.gen(i,20) = 0;                              % APF, area participation factor
end

%% Branch data

numBranch = length(initCase.branch);                % number of branches in the system
mpc.branch = [];                                    % initialize branch data matrix

for i = 1:numBranch
    mpc.branch(i,1) = initCase.branch(i,1);         % f, from bus number
    mpc.branch(i,2) = initCase.branch(i,2);         % t, to bus number
    mpc.branch(i,3) = initCase.branch(i,3);         % r, resistance (p.u.)
    mpc.branch(i,4) = initCase.branch(i,4);         % x, reactance (p.u.)
    mpc.branch(i,5) = initCase.branch(i,5);         % b, total line charging susceptance (p.u.)
    mpc.branch(i,6) = initCase.branch(i,6);         % rateA, MVA rating A (long term rating)
    mpc.branch(i,7) = initCase.branch(i,7);         % rateB, MVA rating B (short term rating)
    mpc.branch(i,8) = initCase.branch(i,8);         % rateC, MVA rating C (emergency rating)
    mpc.branch(i,9) = initCase.branch(i,9);         % ratio, transformer off nominal turns ratio (= 0 for lines)
                                                    % (taps at 'from' bus, impedance a 'to' bus,
                                                    % i.e. if r = x = 0, then ratio = Vf/Vt)
    mpc.branch(i,10) = initCase.branch(i,10);       % angle, transformer phase shift angle (degrees, positive => delay)
    mpc.branch(i,11) = initCase.branch(i,11);       % initial branch status (1 = in service, 0 = out of service)
    mpc.branch(i,12) = initCase.branch(i,12);       % minimum angle difference, angle(Vf) - angle(Vt) (degree)
    mpc.branch(i,13) = initCase.branch(i,13);       % maximum angle difference, angle(Vf) - angle(Vt) (degree)
end

%% Generator cost data

mpc.gencost = [];                                   % initialize generator cost data matrix

for i = 1:numGen
    mpc.gencost(i,1) = initCase.gencost(i,1);       % model (1 = piecewise linear; 2 = polynomial)
    mpc.gencost(i,2) = initCase.gencost(i,2);       % startup, startup cost in US dollars
    mpc.gencost(i,3) = initCase.gencost(i,3);       % shutdown, shutdown cost in US dollars
    mpc.gencost(i,4) = initCase.gencost(i,4);       % N, number of cost coefficients to follow for polynomial cost function
                                                    % or number of data points for piecewise linear
                                                    
                                                    % parameters following define total cost function f(p),
                                                    % units of f and p are $/hr and MV (or MVAr), respectively
                                                    % model = 1 : p0, f0, p1, f1, ..., pn, fn
                                                    % where p0 < p1 < ... < pn and the cost f(p) is defined
                                                    % by the coordinates (p0,f0), (p1,f1), ..., (pn,fn)
                                                    % of the end/break-points of the piecewise linear cost function
                                                    % model = 2 : cn, ..., c1, c0
                                                    % n+1 coefficiencts of an n-th order polynomial cost function,
                                                    % starting with highest order, where cost is
                                                    % f(p) = cn*p^n + ... + c1*p + c0
                                                    
    mpc.gencost(i,5) = initCase.gencost(i,5);
    mpc.gencost(i,6) = initCase.gencost(i,6);
    mpc.gencost(i,7) = initCase.gencost(i,7);
	mpc.gencost(i,8) = 0;
end

for i = (numGen + 1):(numGen + numLoad)
    mpc.gencost(i,1) = 1;                           % model (1 = piecewise linear; 2 = polynomial)
    mpc.gencost(i,2) = 0;                           % startup, startup cost in US dollars
    mpc.gencost(i,3) = 0;                           % shutdown, shutdown cost in US dollars
    mpc.gencost(i,4) = 2;                           % N, number of cost coefficients to follow for polynomial cost function
                                                    % or number of data points for piecewise linear
                                
                                                    % parameters following define total cost function f(p),
                                                    % units of f and p are $/hr and MV (or MVAr), respectively
                                                    % model = 1 : p0, f0, p1, f1, ..., pn, fn
                                                    % where p0 < p1 < ... < pn and the cost f(p) is defined
                                                    % by the coordinates (p0,f0), (p1,f1), ..., (pn,fn)
                                                    % of the end/break-points of the piecewise linear cost function
                                                    % model = 2 : cn, ..., c1, c0
                                                    % n+1 coefficiencts of an n-th order polynomial cost function,
                                                    % starting with highest order, where cost is
                                                    % f(p) = cn*p^n + ... + c1*p + c0
                                                    
    mpc.gencost(i,5) = -initCase.bus(listLoad(i-numGen),3);
    mpc.gencost(i,6) = -100000*initCase.bus(listLoad(i-numGen),3);
    mpc.gencost(i,7) = 0;
    mpc.gencost(i,8) = 0;
end

