import random
import math

a = input()
b = ''
for _ in range(4):
    c = ''
    for _ in range(5):
        c += b
        b += a
d = c + '.'
e = ''
for _ in range(8):
        if _ == 3:
            break
        e += d
f = [e for _ in range(10)]
random.shuffle(f)
g = random.choice(f)
h = g + '3'
i = h + '4'
j = i + '1'
k = f'string {j}'
l = k + '.'
m = ''
for _ in range(2):
    for __ in range(2):
                m += l
n = [m for _ in range(8)]
random.shuffle(n)
o = random.choice(n)
p = o[0:]
if p == p:
    s = p + 'c1'
elif p == '13':
    s = q + 'c2'
else:
    s = r + 'c3'
if s == s:
    v = s + 'c1'
elif s == '11':
    v = t + 'c2'
else:
    v = u + 'c3'
w = [v for _ in range(7)]
random.shuffle(w)
x = random.choice(w)
y = ''
for _ in range(4):
    z = ''
    for _ in range(3):
        aa = ''
        for _ in range(3):
            aa += z
            z += y
        y += x
ab_dict = {50: aa, 56: aa, 45: aa, 3: aa, 71: aa, 82: aa, 1: aa, 18: aa}
ac_dict = {99: ab_dict, 53: ab_dict, 74: ab_dict, 54: ab_dict}
ad_dict = {56: ac_dict, 20: ac_dict, 39: ac_dict, 34: ac_dict, 50: ac_dict}
ae = random.choice(list(ad_dict.values()))
af = random.choice(list(ae.values()))
ag = random.choice(list(af.values()))
ah_list = [ag for _ in range(7)]
ai_list = [ah_list for _ in range(5)]
aj_list = [ai_list for _ in range(2)]
ak = random.choice(aj_list)
al = random.choice(ak)
am = random.choice(al)
an = ''
for _ in range(5):
    ao = ''
    for _ in range(4):
        ao += an
        an += am
ap = ''
for _ in range(3):
    aq = ''
    for _ in range(3):
        ar = ''
        for _ in range(2):
            ar += aq
            aq += ap
        ap += ao
print(ar)