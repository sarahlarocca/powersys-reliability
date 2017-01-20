model.pfn.gam.gamma <- gam(AC ~ s(LCSG.n) + s(D.n) + s(E.n) + s(EN.n) + s(ENE.n) + s(CL.n) + s(PCL.n) + s(PNS.n), family = Gamma, data = pfn.n)

model.pfn.gam.gamma <- gam(AC ~ s(EN.n) + s(PNS.n), family = Gamma, data = pfn.n)

model.pfn.gam.gamma <- gam(AC ~ LCSG.n + D.n + E.n + s(EN.n) + ENE.n + CL.n + PCL.n + s(PNS.n), family = Gamma, data = pfn.n)

