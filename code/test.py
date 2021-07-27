# import stuff
from sympy.interactive import printing
printing.init_printing(use_latex=True)
from sympy import Function
from sympy.solvers import solve
import sympy as sp

X1, X2, LC, s, u = sp.var('X_1, X_2, L_C, s, u', positive=True);

# import exogenous variables

S1, S2, S3, S4, p1, p2, p3, p4, fdash, vdash,f, v = sp.symbols('S_1, S_2, S_3, S_4, p_1, p_2, p_3, p_4, \hat{F}, \hat{V}, f, v', positive=True)

v0 = sp.symbols('v_0') # raw skill level

i = sp.symbols('\hat{\imath}') # children in the household - time endowment
I = sp.symbols('I') # household size and total time endowment

climate = sp.symbols('theta') # climate variable
capital = sp.symbols('K') # quasi fixed land and (non-human) capital

lam = sp.symbols('\lambda') # shadow prices (lagrange multiplier)

# pen-and-paper equation system for Langrangian Optimization

LDash1 = 0.25/(X1-S1)-lam*p1 # L' w.r.t X1
LDash2 = 0.25/(X2-S2)-lam*p2 # L' w.r.t X2
LDash3 = -0.25/-S3 + 2*lam*p3 + lam*p1*fdash # L' w.r.t LA
LDash4 = -1/(i-LC) - 0.25/(i-LC-S4) + 2*lam*p4 + lam*p1*fdash*(-s*u + s*v +(1-s)*v0)
LDash5 = lam*p1*fdash *(-u*LC + v*LC - v0*LC)
LDash6 = lam*p1*fdash *(-s*LC + s*vdash*LC)
LDash7 = p1*X1 + p2*X2 -2*p3*I + 2*p3*i + 2*p3*(I-i) -2*p4*i + 2*p4*LC + p1*f

solve((LDash1,LDash2, LDash3, LDash4, LDash5, LDash6, LDash7), (X1,X2,LC,u,s))