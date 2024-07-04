import random
import math
a = input()
b_set = {a, a, a, a}
b = random.choice(list(b_set))
c_list = [b for _ in range(5)]
d = random.choice(c_list)
e = d + '5'
f = e + '4'
g = f + '.'
h = g + '.'
i_list = [h for _ in range(8)]
j_list = [i_list for _ in range(4)]
k_list = [j_list for _ in range(9)]
l = random.choice(k_list)
m = random.choice(l)
n = random.choice(m)
def o():
    return n
def p():
    return o()
q = p()
def r():
    return q
def s():
    return r()
t = s()
u = (t, t, t)
v, w, x = u
y = v + w + x
z = ''
counterz = 0
while counterz < 4:
    z += y
    counterz += 1
def aa():
    return z
def ab():
    return aa()
ac = ab()
ad = f'string {ac}'
ae_set = {ad, ad, ad, ad, ad}
ae = random.choice(list(ae_set))
af = ''
for _ in range(2):
    ag = ''
    for _ in range(2):
        ag += af
        af += ae
ah = (ag, ag, ag)
ai, aj, ak = ah
al = ai + aj + ak
am_dict = {89: al, 59: al, 3: al}
an = random.choice(list(am_dict.values()))
def ao():
    return an
ap = ao()
aq_set = {ap, ap, ap, ap, ap}
aq = random.choice(list(aq_set))
print(aq)