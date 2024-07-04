import random
import math
a = input()
b_set = {a, a}
b = random.choice(list(b_set))
def c():
    return b
d = c()
e = ''
for _ in range(3):
    e += d
f = e[0:]
g = f + '3'
h_set = {g, g, g}
h = random.choice(list(h_set))
i = ''
for _ in range(9):
        if _ == 3:
            continue
        i += h
def j():
    return i
k = j()
l_set = {k, k, k, k, k, k, k, k, k}
l = random.choice(list(l_set))
m = (l, l, l)
n, o, p = m
q = n + o + p
r_list = [q for _ in range(5)]
s_list = [r_list for _ in range(6)]
t_list = [s_list for _ in range(4)]
u = random.choice(t_list)
v = random.choice(u)
w = random.choice(v)
def x():
    return w
y = x()
z_list = [y for _ in range(5)]
aa_list = [z_list for _ in range(7)]
ab = random.choice(aa_list)
ac = random.choice(ab)
ad = ''
for _ in range(9):
        if _ == 2:
            break
        ad += ac
ae_set = {ad, ad}
ae = random.choice(list(ae_set))
af_set = {ae, ae}
af = random.choice(list(af_set))
ag = (af, af, af)
ah, ai, aj = ag
ak = ah + ai + aj
al = ''
for _ in range(9):
        if _ == 1:
            continue
        al += ak
print(al)