import random
import math
a = input()
b_dict = {15: a, 30: a, 78: a, 14: a, 15: a}
c_dict = {88: b_dict, 63: b_dict, 98: b_dict}
d_dict = {90: c_dict, 16: c_dict, 88: c_dict, 76: c_dict, 47: c_dict, 84: c_dict}
e = random.choice(list(d_dict.values()))
f = random.choice(list(e.values()))
g = random.choice(list(f.values()))
h_set = {g, g, g, g, g, g, g, g, g}
h = random.choice(list(h_set))
i = ''
for _ in range(2):
    j = ''
    for _ in range(2):
        k = ''
        for _ in range(2):
            k += j
            j += i
        i += h
l_dict = {95: k, 18: k, 36: k, 96: k, 64: k, 97: k, 16: k, 95: k}
m_dict = {35: l_dict, 58: l_dict, 66: l_dict, 15: l_dict}
n_dict = {64: m_dict, 38: m_dict}
o = random.choice(list(n_dict.values()))
p = random.choice(list(o.values()))
q = random.choice(list(p.values()))
r = q + '9'
s = r + '4'
t = s + '1'
def u():
    return t
def v():
    return u()
def w():
    return v()
x = w()
y = [x for _ in range(6)]
random.shuffle(y)
z = random.choice(y)
aa_dict = {82: z, 58: z, 53: z}
ab_dict = {82: aa_dict, 78: aa_dict, 62: aa_dict, 83: aa_dict, 9: aa_dict, 46: aa_dict}
ac_dict = {7: ab_dict, 47: ab_dict, 23: ab_dict}
ad = random.choice(list(ac_dict.values()))
ae = random.choice(list(ad.values()))
af = random.choice(list(ae.values()))
if af == '10':
    ag = af + ' c1'
elif af == '14':
    ag = af + ' c2'
else:
    ag = af + ' c3'
ah = ''
for _ in range(2):
    for __ in range(4):
                ah += ag
def ai():
    return ah
aj = ai()
ak_list = [aj for _ in range(4)]
al_list = [ak_list for _ in range(8)]
am_list = [al_list for _ in range(2)]
an = random.choice(am_list)
ao = random.choice(an)
ap = random.choice(ao)
aq = ap[0:]
ar = ''
counterar = 0
while counterar < 3:
    ar += aq
    counterar += 1
at = ''
for _ in range(5):
    for __ in range(3):
                at += ar
au = ''
for _ in range(5):
    av = ''
    for _ in range(5):
        aw = ''
        for _ in range(4):
            aw += av
            av += au
        au += at
ax_set = {aw, aw, aw, aw, aw, aw, aw, aw, aw, aw}
ax = random.choice(list(ax_set))
ay = ax + '2'
az = ay + '7'
print(az)