import random
import math

a = input()
b = a + '3'
c_list = [b for _ in range(7)]
d_list = [c_list for _ in range(4)]
e = random.choice(d_list)
f = random.choice(e)
g = [f for _ in range(10)]
random.shuffle(g)
h = random.choice(g)
i = h[0:]
j_list = [i for _ in range(10)]
k_list = [j_list for _ in range(2)]
l_list = [k_list for _ in range(8)]
m = random.choice(l_list)
n = random.choice(m)
o = random.choice(n)
p_dict = {77: o, 53: o, 78: o, 73: o, 84: o}
q_dict = {3: p_dict, 95: p_dict, 45: p_dict, 52: p_dict, 32: p_dict, 5: p_dict, 44: p_dict}
r_dict = {4: q_dict, 41: q_dict, 95: q_dict, 84: q_dict, 59: q_dict, 68: q_dict}
s = random.choice(list(r_dict.values()))
t = random.choice(list(s.values()))
u = random.choice(list(t.values()))
def v():
    return u
def w():
    return v()
def x():
    return w()
y = x()
if y == y:
    ab = y + 'c1'
elif y == '19':
    ab = z + 'c2'
else:
    ab = aa + 'c3'
ac = ab + '4'
ad_set = {ac, ac}
ad = random.choice(list(ad_set))
ae_dict = {45: ad, 65: ad, 20: ad, 2: ad, 35: ad, 78: ad, 7: ad, 63: ad, 17: ad}
af_dict = {60: ae_dict, 39: ae_dict, 6: ae_dict, 78: ae_dict, 92: ae_dict, 97: ae_dict, 97: ae_dict, 4: ae_dict, 59: ae_dict, 10: ae_dict}
ag_dict = {12: af_dict, 54: af_dict, 92: af_dict, 37: af_dict, 3: af_dict, 5: af_dict, 85: af_dict, 57: af_dict, 41: af_dict, 96: af_dict}
ah = random.choice(list(ag_dict.values()))
ai = random.choice(list(ah.values()))
aj = random.choice(list(ai.values()))
ak_set = {aj, aj, aj, aj, aj, aj}
ak = random.choice(list(ak_set))
def al():
    return ak
def am():
    return al()
an = am()
ao = an + '.'
ap = (ao, ao, ao)
aq, ar, at = ap
au = aq + ar + at
av = au + '8'
aw = av + '3'
ax = f'string {aw}'
ay = ''
for _ in range(2):
    az = ''
    for _ in range(3):
        ba = ''
        for _ in range(4):
            ba += az
            az += ay
        ay += ax
print(ba)