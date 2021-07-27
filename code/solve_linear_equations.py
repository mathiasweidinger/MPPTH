# import stuff
from sympy.interactive import printing
printing.init_printing(use_latex=True)
from sympy import Function
from sympy.solvers import solve
import sympy as sp

lam, c, e = sp.var('lambda, c, e', positive=True);

# import exogenous variables

s, I, i, bet, gam, P, p = sp.symbols('s, I, i, beta, gamma, P, p', positive=True)

# pen-and-paper equation system for Langrangian Optimization

### CASE ONE

LDash1 = 1-e + lam*((I-i)+i*bet)
LDash2 = c+s+lam*(-p*gam*i)
LDash3 = (I-i)*c+i*bet*c-P

solution1 = solve((LDash1,LDash2, LDash3), (lam, c, e))

### CASE TWO

LDash4 = 1-lam*((I-i)+i*bet)
LDash5 = lam*p*gam*i
LDash6 = LDash3 = (I-i)*c+i*bet*c-P

solution2 = solve((LDash4, LDash5, LDash6), (lam, c, e))

print(solution1, solution2)