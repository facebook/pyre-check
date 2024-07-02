import random
import math
a = input()
b = a + '7'
c = b + '3'
d = c + '3'
e = ''
countere = 0
while countere < 2:
    e += d
    countere += 1
f = ''
counterf = 0
while counterf < 2:
    f += e
    counterf += 1
def g():
    return f
def h():
    return g()
def i():
    return h()
j = i()
k = (j, j, j)
l, m, n = k
o = l + m + n
p = ''
counterp = 0
while counterp < 3:
    p += o
    counterp += 1
q_list = [p for _ in range(9)]
r_list = [q_list for _ in range(10)]
s = random.choice(r_list)
t = random.choice(s)
u_list = [t for _ in range(3)]
v = random.choice(u_list)
if v == '9':
    w = v + ' c1'
elif v == '15':
    w = v + ' c2'
else:
    w = v + ' c3'
def x():
    return w
y = x()
z_set = {y, y, y, y}
z = random.choice(list(z_set))
aa = z + '.'
if aa == '5':
    ab = aa + ' c1'
elif aa == '17':
    ab = aa + ' c2'
else:
    ab = aa + ' c3'
ac_list = [ab for _ in range(6)]
ad_list = [ac_list for _ in range(10)]
ae = random.choice(ad_list)
af = random.choice(ae)
ag_set = {af, af, af, af, af, af, af, af, af, af}
ag = random.choice(list(ag_set))
ah = [ag for _ in range(5)]
random.shuffle(ah)
ai = random.choice(ah)
aj = ''
for _ in range(4):
    aj += ai
ak = (aj, aj, aj)
al, am, an = ak
ao = al + am + an
print(ao)