import random
import math
a = input()
b = [a for _ in range(7)]
random.shuffle(b)
c = random.choice(b)
d = f'string {c}'
e_dict = {56: d, 91: d, 37: d, 56: d, 95: d, 60: d}
f_dict = {69: e_dict, 26: e_dict, 33: e_dict, 64: e_dict, 62: e_dict, 42: e_dict, 96: e_dict}
g_dict = {95: f_dict, 91: f_dict, 92: f_dict}
h = random.choice(list(g_dict.values()))
i = random.choice(list(h.values()))
j = random.choice(list(i.values()))
k = f'string {j}'
l = (k, k, k)
m, n, o = l
p = m + n + o
q = p[0:]
r = (q, q, q)
s, t, u = r
v = s + t + u
w = v + '7'
x = w + '4'
y = f'string {x}'
z = ''
for _ in range(3):
    aa = ''
    for _ in range(4):
        aa += z
        z += y
def ab():
    return aa
def ac():
    return ab()
def ad():
    return ac()
ae = ad()
af = [ae for _ in range(5)]
random.shuffle(af)
ag = random.choice(af)
ah = (ag, ag, ag)
ai, aj, ak = ah
al = ai + aj + ak
am_dict = {35: al, 25: al, 22: al, 78: al}
an_dict = {8: am_dict, 20: am_dict, 67: am_dict, 23: am_dict, 31: am_dict, 48: am_dict}
ao = random.choice(list(an_dict.values()))
ap = random.choice(list(ao.values()))
aq = ap[0:]
ar_dict = {8: aq, 89: aq, 48: aq, 51: aq, 14: aq, 69: aq, 93: aq, 67: aq, 80: aq, 63: aq}
at_dict = {8: ar_dict, 6: ar_dict, 27: ar_dict, 44: ar_dict, 83: ar_dict, 69: ar_dict, 73: ar_dict, 82: ar_dict}
au_dict = {10: at_dict, 57: at_dict, 44: at_dict, 59: at_dict, 28: at_dict, 36: at_dict, 28: at_dict}
av = random.choice(list(au_dict.values()))
aw = random.choice(list(av.values()))
ax = random.choice(list(aw.values()))
ay = ax[0:]
az_set = {ay, ay, ay, ay, ay, ay, ay, ay, ay}
az = random.choice(list(az_set))
print(az)