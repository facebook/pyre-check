import random
import math
a = input()
b = ''
for _ in range(2):
    for __ in range(2):
                b += a
def c():
    return b
def d():
    return c()
def e():
    return d()
f = e()
g = f[0:]
h = g + '.'
def i():
    return h
def j():
    return i()
k = j()
l_dict = {72: k, 65: k, 1: k, 38: k, 29: k, 92: k, 29: k, 74: k, 7: k, 30: k}
m_dict = {21: l_dict, 49: l_dict, 83: l_dict, 3: l_dict}
n_dict = {88: m_dict, 56: m_dict, 6: m_dict, 21: m_dict, 58: m_dict, 65: m_dict, 32: m_dict, 31: m_dict}
o = random.choice(list(n_dict.values()))
p = random.choice(list(o.values()))
q = random.choice(list(p.values()))
if q == '8':
    r = q + ' c1'
elif q == '16':
    r = q + ' c2'
else:
    r = q + ' c3'
s_set = {r, r, r}
s = random.choice(list(s_set))
t = ''
for _ in range(10):
        if _ == 1:
            continue
        t += s
u_dict = {73: t, 95: t, 1: t, 40: t, 68: t, 27: t, 99: t, 64: t, 46: t, 90: t}
v_dict = {68: u_dict, 93: u_dict, 16: u_dict, 98: u_dict, 36: u_dict}
w = random.choice(list(v_dict.values()))
x = random.choice(list(w.values()))
def y():
    return x
def z():
    return y()
aa = z()
ab = aa + '.'
ac = [ab for _ in range(9)]
random.shuffle(ac)
ad = random.choice(ac)
ae = ad + '.'
af_set = {ae, ae, ae, ae, ae, ae, ae, ae, ae, ae}
af = random.choice(list(af_set))
ag = ''
for _ in range(8):
        if _ == 4:
            continue
        ag += af
ah_dict = {94: ag, 23: ag, 52: ag, 97: ag}
ai = random.choice(list(ah_dict.values()))
aj_list = [ai for _ in range(9)]
ak_list = [aj_list for _ in range(7)]
al = random.choice(ak_list)
am = random.choice(al)
print(am)