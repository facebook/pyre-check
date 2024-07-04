import random
import math
a = input()
b = ''
for _ in range(8):
        if _ == 5:
            continue
        b += a
c = ''
for _ in range(2):
    d = ''
    for _ in range(2):
        e = ''
        for _ in range(2):
            e += d
            d += c
        c += b
def f():
    return e
def g():
    return f()
h = g()
i = (h, h, h)
j, k, l = i
m = j + k + l
n = m[0:]
o_dict = {34: n, 74: n}
p_dict = {86: o_dict, 46: o_dict, 24: o_dict, 86: o_dict}
q_dict = {33: p_dict, 91: p_dict, 61: p_dict, 26: p_dict, 48: p_dict, 94: p_dict}
r = random.choice(list(q_dict.values()))
s = random.choice(list(r.values()))
t = random.choice(list(s.values()))
def u():
    return t
def v():
    return u()
def w():
    return v()
x = w()
y = (x, x, x)
z, aa, ab = y
ac = z + aa + ab
ad = ''
for _ in range(5):
        if _ == 3:
            break
        ad += ac
ae_set = {ad, ad, ad, ad, ad, ad, ad, ad, ad}
ae = random.choice(list(ae_set))
af = ''
for _ in range(10):
        if _ == 5:
            continue
        af += ae
ag_set = {af, af, af, af, af, af, af, af, af}
ag = random.choice(list(ag_set))
ah = ''
for _ in range(4):
    ai = ''
    for _ in range(3):
        aj = ''
        for _ in range(3):
            aj += ai
            ai += ah
        ah += ag
ak = aj + '9'
al = ak + '3'
am = [al for _ in range(10)]
random.shuffle(am)
an = random.choice(am)
ao = [an for _ in range(9)]
random.shuffle(ao)
ap = random.choice(ao)
def aq():
    return ap
def ar():
    return aq()
def at():
    return ar()
au = at()
av = au + '5'
aw = av + '2'
ax = aw + '8'
print(ax)