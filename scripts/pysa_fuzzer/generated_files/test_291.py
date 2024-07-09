import random
import math

a = input()
b = f'string {a}'
c = (b, b, b)
d, e, f = c
g = d + e + f
h = (g, g, g)
i, j, k = h
l = i + j + k
m_list = [l for _ in range(3)]
n_list = [m_list for _ in range(5)]
o_list = [n_list for _ in range(10)]
p = random.choice(o_list)
q = random.choice(p)
r = random.choice(q)
s = (r, r, r)
t, u, v = s
w = t + u + v
x = w[0:]
def y():
    return x
def z():
    return y()
def aa():
    return z()
ab = aa()
ac = ab[0:]
ad = ac + '.'
ae = f'string {ad}'
af = ''
for _ in range(5):
    for __ in range(2):
                af += ae
ag = ''
counterag = 0
while counterag < 5:
    ah = ''
    counterah = 0
    while counterah < 4:
        ai = ''
        counterai = 0
        while counterai < 5:
            ai += ah
            counterai += 1
            ah += ag
            counterah += 1
        ag += af
        counterag += 1
aj = ''
counteraj = 0
while counteraj < 5:
    ak = ''
    counterak = 0
    while counterak < 2:
        ak += aj
        counterak += 1
        aj += ai
        counteraj += 1
al = [ak for _ in range(7)]
random.shuffle(al)
am = random.choice(al)
an = am + '.'
ao = an + '.'
ap = ao + '.'
aq = ap + '.'
print(aq)