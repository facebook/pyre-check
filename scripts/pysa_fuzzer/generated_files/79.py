import random
import math
a = input()
b_dict = {20: a, 9: a, 15: a, 78: a, 83: a}
c_dict = {71: b_dict, 73: b_dict, 23: b_dict, 17: b_dict}
d_dict = {11: c_dict, 25: c_dict, 74: c_dict, 26: c_dict, 12: c_dict, 65: c_dict, 77: c_dict, 58: c_dict, 64: c_dict, 91: c_dict}
e = random.choice(list(d_dict.values()))
f = random.choice(list(e.values()))
g = random.choice(list(f.values()))
h = g[0:]
i = ''
for _ in range(4):
    j = ''
    for _ in range(5):
        j += i
        i += h
k_list = [j for _ in range(5)]
l = random.choice(k_list)
def m():
    return l
n = m()
o_set = {n, n, n, n, n, n}
o = random.choice(list(o_set))
p = ''
counterp = 0
while counterp < 3:
    p += o
    counterp += 1
q = ''
for _ in range(5):
    q += p
r = ''
for _ in range(5):
    for __ in range(4):
                r += q
s_dict = {5: r, 27: r, 23: r, 33: r, 59: r, 4: r, 35: r, 86: r, 69: r}
t_dict = {9: s_dict, 53: s_dict, 60: s_dict}
u_dict = {41: t_dict, 69: t_dict, 19: t_dict, 32: t_dict, 14: t_dict, 89: t_dict, 27: t_dict, 13: t_dict}
v = random.choice(list(u_dict.values()))
w = random.choice(list(v.values()))
x = random.choice(list(w.values()))
y = x + '9'
z = y + '1'
aa = f'string {z}'
ab = ''
counterab = 0
while counterab < 3:
    ac = ''
    counterac = 0
    while counterac < 2:
        ad = ''
        counterad = 0
        while counterad < 4:
            ad += ac
            counterad += 1
            ac += ab
            counterac += 1
        ab += aa
        counterab += 1
ae = ''
for _ in range(5):
    ae += ad
af = ae[0:]
ag = (af, af, af)
ah, ai, aj = ag
ak = ah + ai + aj
def al():
    return ak
am = al()
an_dict = {42: am, 30: am, 6: am}
ao_dict = {76: an_dict, 16: an_dict, 87: an_dict, 59: an_dict, 76: an_dict, 94: an_dict, 16: an_dict}
ap_dict = {28: ao_dict, 96: ao_dict, 6: ao_dict}
aq = random.choice(list(ap_dict.values()))
ar = random.choice(list(aq.values()))
at = random.choice(list(ar.values()))
print(at)