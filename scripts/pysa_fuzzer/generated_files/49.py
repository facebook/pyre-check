import random
import math
a = input()
b = ''
for _ in range(5):
    for __ in range(5):
                b += a
def c():
    return b
def d():
    return c()
e = d()
f = ''
counterf = 0
while counterf < 4:
    g = ''
    counterg = 0
    while counterg < 2:
        g += f
        counterg += 1
        f += e
        counterf += 1
h = g[0:]
i = (h, h, h)
j, k, l = i
m = j + k + l
n = ''
countern = 0
while countern < 5:
    n += m
    countern += 1
o = f'string {n}'
p = o + '.'
q = ''
for _ in range(4):
    r = ''
    for _ in range(3):
        s = ''
        for _ in range(4):
            s += r
            r += q
        q += p
t_list = [s for _ in range(5)]
u_list = [t_list for _ in range(4)]
v_list = [u_list for _ in range(4)]
w = random.choice(v_list)
x = random.choice(w)
y = random.choice(x)
z = f'string {y}'
def aa():
    return z
def ab():
    return aa()
ac = ab()
ad = [ac for _ in range(10)]
random.shuffle(ad)
ae = random.choice(ad)
af = ae + '9'
ag = (af, af, af)
ah, ai, aj = ag
ak = ah + ai + aj
al = ak + '4'
am = al + '3'
an = ''
for _ in range(10):
        if _ == 5:
            continue
        an += am
if an == '4':
    ao = an + ' c1'
elif an == '11':
    ao = an + ' c2'
else:
    ao = an + ' c3'
print(ao)