import random
import math

a = input()
b = a + '.'
c = ''
for _ in range(2):
    c += b
d = [c for _ in range(7)]
random.shuffle(d)
e = random.choice(d)
f = (e, e, e)
g, h, i = f
j = g + h + i
k_list = [j for _ in range(4)]
l_list = [k_list for _ in range(8)]
m_list = [l_list for _ in range(4)]
n = random.choice(m_list)
o = random.choice(n)
p = random.choice(o)
q = (p, p, p)
r, s, t = q
u = r + s + t
def v():
    return u
w = v()
x = w + '9'
y = x + '.'
z = [y for _ in range(5)]
random.shuffle(z)
aa = random.choice(z)
ab = aa + '.'
ac_list = [ab for _ in range(7)]
ad = random.choice(ac_list)
ae = ad[0:]
af_set = {ae, ae, ae, ae, ae, ae}
af = random.choice(list(af_set))
ag_set = {af, af, af, af, af, af, af, af}
ag = random.choice(list(ag_set))
def ah():
    return ag
def ai():
    return ah()
def aj():
    return ai()
ak = aj()
al = ak[0:]
am = [al for _ in range(9)]
random.shuffle(am)
an = random.choice(am)
print(an)