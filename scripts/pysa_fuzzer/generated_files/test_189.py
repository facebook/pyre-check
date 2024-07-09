import random
import math

a = input()
def b():
    return a
c = b()
d = ''
for _ in range(10):
        if _ == 2:
            continue
        d += c
e = ''
for _ in range(8):
        if _ == 4:
            continue
        e += d
f_dict = {84: e, 81: e, 38: e, 44: e, 23: e, 21: e}
g_dict = {35: f_dict, 48: f_dict, 25: f_dict}
h_dict = {76: g_dict, 81: g_dict}
i = random.choice(list(h_dict.values()))
j = random.choice(list(i.values()))
k = random.choice(list(j.values()))
l_dict = {70: k, 54: k, 11: k, 10: k, 9: k, 13: k, 45: k, 71: k, 52: k}
m = random.choice(list(l_dict.values()))
n = ''
for _ in range(4):
    o = ''
    for _ in range(5):
        o += n
        n += m
p = f'string {o}'
q = (p, p, p)
r, s, t = q
u = r + s + t
v = ''
for _ in range(8):
        if _ == 1:
            continue
        v += u
w = ''
for _ in range(2):
    x = ''
    for _ in range(3):
        y = ''
        for _ in range(2):
            y += x
            x += w
        w += v
z_dict = {2: y, 62: y, 87: y}
aa_dict = {88: z_dict, 20: z_dict, 49: z_dict, 84: z_dict, 68: z_dict, 20: z_dict, 56: z_dict, 19: z_dict, 98: z_dict, 47: z_dict}
ab = random.choice(list(aa_dict.values()))
ac = random.choice(list(ab.values()))
ad_set = {ac, ac, ac, ac, ac}
ad = random.choice(list(ad_set))
ae = ''
for _ in range(4):
    for __ in range(2):
                ae += ad
if ae == ae:
    ah = ae + 'c1'
elif ae == '18':
    ah = af + 'c2'
else:
    ah = ag + 'c3'
ai_set = {ah, ah, ah, ah, ah, ah, ah, ah, ah}
ai = random.choice(list(ai_set))
aj = [ai for _ in range(5)]
random.shuffle(aj)
ak = random.choice(aj)
al = f'string {ak}'
if al == al:
    ao = al + 'c1'
elif al == '15':
    ao = am + 'c2'
else:
    ao = an + 'c3'
print(ao)