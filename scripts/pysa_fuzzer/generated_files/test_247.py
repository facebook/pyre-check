import random
import math

a = input()
b = a[0:]
c = ''
for _ in range(5):
    c += b
def d():
    return c
def e():
    return d()
def f():
    return e()
g = f()
def h():
    return g
def i():
    return h()
j = i()
k = ''
for _ in range(4):
    k += j
l = k + '.'
m = f'string {l}'
n_list = [m for _ in range(6)]
o = random.choice(n_list)
p = o + '.'
q = ''
for _ in range(9):
        if _ == 2:
            continue
        q += p
r = ''
for _ in range(4):
    s = ''
    for _ in range(3):
        s += r
        r += q
t_list = [s for _ in range(2)]
u_list = [t_list for _ in range(2)]
v_list = [u_list for _ in range(7)]
w = random.choice(v_list)
x = random.choice(w)
y = random.choice(x)
z = (y, y, y)
aa, ab, ac = z
ad = aa + ab + ac
ae = ad[0:]
af = ''
counteraf = 0
while counteraf < 2:
    ag = ''
    counterag = 0
    while counterag < 2:
        ag += af
        counterag += 1
        af += ae
        counteraf += 1
ah = ''
counterah = 0
while counterah < 5:
    ai = ''
    counterai = 0
    while counterai < 3:
        aj = ''
        counteraj = 0
        while counteraj < 3:
            aj += ai
            counteraj += 1
            ai += ah
            counterai += 1
        ah += ag
        counterah += 1
ak = (aj, aj, aj)
al, am, an = ak
ao = al + am + an
ap = ao + '.'
print(ap)