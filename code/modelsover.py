# import stuff
from sympy.interactive import printing
printing.init_printing(use_latex=True)
from sympy import Function
import sympy as sp

# define the maximization variables
x1, x2, x3, x4, lc, la, s, u = sp.var('x_1, x_2, x_3, x_4, L_c, L_a, s, u', real=True);

# import exogenous variables
a1 = sp.symbols('alpha_1')
a2 = sp.symbols('alpha_2')
a3 = sp.symbols('alpha_3')
a4 = sp.symbols('alpha_4')
s1 = sp.symbols('s_1')
s2 = sp.symbols('s_2')
s3 = sp.symbols('s_3')
s4 = sp.symbols('s_4')
v0 = sp.symbols('v_0')
i = sp.symbols('\hat{\imath}')
p1 = sp.symbols('p_1')
p2 = sp.symbols('p_2')
p3 = sp.symbols('p_3')
p4 = sp.symbols('p_4')
I = sp.symbols('I')
lam = sp.symbols('lambda')

# technology of skill enhancement
v = Function('v')(u)
# labour equivalence function (cases 1 and 2)
L1 = la-s * u * lc + s * v * lc + (1 - s) * v0 * lc
L2 = v * (la/u)+v0*(lc-la/u)

# define production function
f1 = Function('f')(L1)
f2 = Function('f')(L2)

# utility function (cases 1 and 2)
U1 = (i-lc) * (x1-s1)**a1 * (x2-s2)**a2 * (x3-s3)**a3* (x4-s4)**a4 
U2 = (x1-s1)**a1 * (x2-s2)**a2 * (x3-s3)**a3* (x4-s4)**a4

# constraints
G1 = p1 * x1 + p2 * x2 + p3 * x3 + p4 * x4 - p3*f1 + p2*L1 - p2*I
G2 = p1 * x1 + p2 * x2 + p3 * x3 + p4 * x4 - p3*f2 + p2*L2 - p2*I

# Lagrangians

Lagrange1 = U1 - lam * G1
Lagrange2 = U2 - lam * G1
Lagrange3 = U1 - lam * G2
Lagrange4 = U2 - lam * G2

grad1 = [sp.diff(Lagrange1,c) for c in [x1, x2, x3, x4, lc, la, s, u]] # gradient of Lagrangian w.r.t choice variables
KKT1 = grad1 + [G1]

grad2 = [sp.diff(Lagrange2,c) for c in [x1, x2, x3, x4, lc, la, s, u]] # gradient of Lagrangian w.r.t choice variables
KKT2 = grad2 + [G1]

grad3 = [sp.diff(Lagrange3,c) for c in [x1, x2, x3, x4, lc, la, s, u]] # gradient of Lagrangian w.r.t choice variables
KKT3 = grad3 + [G2]

grad4 = [sp.diff(Lagrange4,c) for c in [x1, x2, x3, x4, lc, la, s, u]] # gradient of Lagrangian w.r.t choice variables
KKT4 = grad4 + [G2]

candidates = sp.solve(KKT1, [x1, x2, x3, x4, lc, la, s, u, lam], dict=True) # solve the KKT equations
print(candidates)
