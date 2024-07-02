import random
import math
a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g = ''
for _ in range(5):
    h = ''
    for _ in range(5):
        i = ''
        for _ in range(2):
            i += h
            h += g
        g += f
j = ''
for _ in range(2):
    for __ in range(4):
                j += i
k_set = {j, j, j, j, j, j, j, j}
k = random.choice(list(k_set))
l = k + '8'
m = l + '7'
n = m[0:]
if n == '1':
    o = n + ' c1'
elif n == '14':
    o = n + ' c2'
else:
    o = n + ' c3'
p = o[0:]
q = p + '8'
r = q + '7'
if r == '8':
    s = r + ' c1'
elif r == '12':
    s = r + ' c2'
else:
    s = r + ' c3'
t_dict = {61: s, 72: s, 72: s, 42: s, 75: s, 47: s, 90: s}
u_dict = {91: t_dict, 25: t_dict, 61: t_dict, 51: t_dict, 42: t_dict, 79: t_dict, 37: t_dict, 77: t_dict}
v_dict = {11: u_dict, 68: u_dict, 19: u_dict}
w = random.choice(list(v_dict.values()))
x = random.choice(list(w.values()))
y = random.choice(list(x.values()))
z = y[0:]
aa = (z, z, z)
ab, ac, ad = aa
ae = ab + ac + ad
af_dict = {74: ae, 7: ae, 20: ae, 64: ae, 4: ae, 38: ae, 49: ae, 52: ae, 36: ae, 37: ae}
ag_dict = {18: af_dict, 66: af_dict, 66: af_dict, 16: af_dict}
ah = random.choice(list(ag_dict.values()))
ai = random.choice(list(ah.values()))
aj = ai + '7'
ak = aj + '9'
al = ak + '1'
am = [al for _ in range(10)]
random.shuffle(am)
an = random.choice(am)
ao = an + '6'
ap = [ao for _ in range(8)]
random.shuffle(ap)
aq = random.choice(ap)
print(aq)