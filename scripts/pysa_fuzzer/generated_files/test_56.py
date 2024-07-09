import random
import math

a = input()
b = a[0:]
c = (b, b, b)
d, e, f = c
g = d + e + f
h_list = [g for _ in range(7)]
i_list = [h_list for _ in range(5)]
j = random.choice(i_list)
k = random.choice(j)
l = ''
for _ in range(4):
    for __ in range(2):
                l += k
def m():
    return l
def n():
    return m()
def o():
    return n()
p = o()
def q():
    return p
r = q()
s_set = {r, r, r, r, r, r, r, r, r, r}
s = random.choice(list(s_set))
t = f'string {s}'
u = t + '3'
v = ''
counterv = 0
while counterv < 2:
    w = ''
    counterw = 0
    while counterw < 3:
        w += v
        counterw += 1
        v += u
        counterv += 1
x_list = [w for _ in range(5)]
y_list = [x_list for _ in range(10)]
z_list = [y_list for _ in range(4)]
aa = random.choice(z_list)
ab = random.choice(aa)
ac = random.choice(ab)
ad = f'string {ac}'
ae = ''
for _ in range(2):
    for __ in range(5):
                ae += ad
af = (ae, ae, ae)
ag, ah, ai = af
aj = ag + ah + ai
ak_dict = {68: aj, 59: aj, 38: aj, 82: aj, 79: aj, 55: aj, 79: aj, 9: aj, 99: aj}
al_dict = {78: ak_dict, 17: ak_dict, 82: ak_dict}
am_dict = {4: al_dict, 48: al_dict, 21: al_dict, 53: al_dict, 26: al_dict, 81: al_dict, 71: al_dict}
an = random.choice(list(am_dict.values()))
ao = random.choice(list(an.values()))
ap = random.choice(list(ao.values()))
aq = ap + '7'
ar = aq + '3'
at = ar + '4'
au = at + '.'
av = f'string {au}'
print(av)