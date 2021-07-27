# import stuff
from sympy.interactive import printing
printing.init_printing(use_latex=True)
from sympy import Function
from sympy.solvers import solve
import sympy as sp

# define the maximization variables
X1, X2, LC, LA, s, u = sp.var('X_1, X_2, L_C, L_A, s, u', positive=True);

# import exogenous variables

a1 = sp.symbols('alpha_1') # alphas
a2 = sp.symbols('alpha_2')
a3 = sp.symbols('alpha_3')
a4 = sp.symbols('alpha_4')

S1 = sp.symbols('S_1') # Subsistence levels
S2 = sp.symbols('S_2')
S3 = sp.symbols('S_3')
S4 = sp.symbols('S_4')

v0 = sp.symbols('v_0') # raw skill level

i = sp.symbols('\hat{\imath}') # children in the household - time endowment
I = sp.symbols('I') # household size and total time endowment

p1 = sp.symbols('p_1') # price vector
p2 = sp.symbols('p_2')
p3 = sp.symbols('p_3')
p4 = sp.symbols('p_4')

climate = sp.symbols('theta') # climate variable
capital = sp.symbols('K') # quasi fixed land and (non-human) capital

lam = sp.symbols('\lambda') # shadow prices (lagrange multiplier)

# technology of skill enhancement
v = Function('v')(u)

# labour equivalence function (cases 1 and 2)
L1 = Function('L_1')(u, s, LA, LC)
L1 = LA-s * u * LC + s * v * LC + (1 - s) * v0 * LC
L2 = Function('L_2')(u, LA, LC)
L2 = v * (LA/u)+v0*(LC-LA/u)

# define production function
f1 = Function('f')(L1, capital,climate) # positive monotonic and convex in L1, capital, and climate
f2 = Function('f')(L2,capital, climate)

# utility function (cases 1 and 2)
U1 = Function ('U_1')(X1, X2, LA, LC)
U2 = Function ('U_2')(X1, X2, LA, LC)
U1 = (i-LC) * (X1-S1)**a1 *(X2-S2)**a2 * (I-i-LA-S3)**a3 * (i-LC-S4)**a4 
U2 =          (X1-S1)**a1 *(X2-S2)**a2 * (I-i-LA-S3)**a3 * (i-LC-S4)**a4

# main constraints
#G1 = Function('G_1')(X1, X2, LA, LC, f1)
G1 = p1*X1 + p2*X2 + p3*I - p3*i -p3*LA + p4*i - p4*LC - p1*f1 + p3*LA + p4*LC - p3*I-p3*i - p4*i
#G1 = Function('G_1')(X1, X2, LA, LC, f2)
G2 = p1*X1 + p2*X2 + p3*I - p3*i -p3*LA + p4*i - p4*LC - p1*f2 + p3*LA + p4*LC - p3*I-p3*i - p4*i

# Lagrangians
Lagrange1 = U1 - lam * G1 # case 1
Lagrange2 = U2 - lam * G1 # case 2
Lagrange3 = U1 - lam * G2 # case 3
Lagrange4 = U2 - lam * G2 # case 4

# Calculate KKT-conditions
grad1 = [sp.diff(Lagrange1,c) for c in [X1, X2, X3, X4, LC, LA, s, u]] # gradient of Lagrangian w.r.t choice variables
KKT1 = grad1 + [G1]

grad2 = [sp.diff(Lagrange2,c) for c in [X1, X2, X3, X4, LC, LA, s, u]] # gradient of Lagrangian w.r.t choice variables
KKT2 = grad2 + [G1]

grad3 = [sp.diff(Lagrange3,c) for c in [X1, X2, X3, X4, LC, LA, s, u]] # gradient of Lagrangian w.r.t choice variables
KKT3 = grad3 + [G2]

grad4 = [sp.diff(Lagrange4,c) for c in [X1, X2, X3, X4, LC, LA, s, u]] # gradient of Lagrangian w.r.t choice variables
KKT4 = grad4 + [G2]

### Here the thing breaks down - perhaps because the convexity of f1 is not specified.
########################################################################################################
candidates1 = sp.solve(KKT1, [X1, X2, X3, X4, LC, LA, s, u, lam], dict=True) # solve the KKT equations #
candidates2 = sp.solve(KKT2, [X1, X2, X3, X4, LC, LA, s, u, lam], dict=True) # solve the KKT equations #
candidates3 = sp.solve(KKT3, [X1, X2, X3, X4, LC, LA, s, u, lam], dict=True) # solve the KKT equations #
candidates4 = sp.solve(KKT4, [X1, X2, X3, X4, LC, LA, s, u, lam], dict=True) # solve the KKT equations #
########################################################################################################


# return candidates for optima
print(candidates)
