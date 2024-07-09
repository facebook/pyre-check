import random
import math

a = input()
b = [a for _ in range(6)]
random.shuffle(b)
c = random.choice(b)
d = c[0:]
e = ''
for _ in range(2):
    e += d
f = (e, e, e)
g, h, i = f
j = g + h + i
k_dict = {15: j, 67: j, 10: j, 50: j, 63: j, 67: j}
l = random.choice(list(k_dict.values()))
m = ''
for _ in range(3):
    n = ''
    for _ in range(2):
        n += m
        m += l
o_dict = {54: n, 57: n, 49: n, 41: n, 32: n, 29: n, 2: n, 58: n, 85: n, 31: n}
p_dict = {97: o_dict, 12: o_dict, 63: o_dict, 90: o_dict, 15: o_dict, 19: o_dict, 91: o_dict, 19: o_dict}
q = random.choice(list(p_dict.values()))
r = random.choice(list(q.values()))
def s():
    return r
def t():
    return s()
def u():
    return t()
v = u()
w = ''
for _ in range(2):
    for __ in range(4):
                w += v
x = ''
for _ in range(2):
    for __ in range(3):
                x += w
y = (x, x, x)
z, aa, ab = y
ac = z + aa + ab
ad_dict = {3: ac, 24: ac, 92: ac, 7: ac, 88: ac, 5: ac, 9: ac}
ae_dict = {54: ad_dict, 19: ad_dict, 19: ad_dict, 62: ad_dict}
af = random.choice(list(ae_dict.values()))
ag = random.choice(list(af.values()))
ah = f'string {ag}'
ai = ''
for _ in range(5):
    for __ in range(3):
                ai += ah
aj = [ai for _ in range(8)]
random.shuffle(aj)
ak = random.choice(aj)
al = [ak for _ in range(9)]
random.shuffle(al)
am = random.choice(al)
an = ''
for _ in range(3):
    for __ in range(2):
                an += am
ao_dict = {9: an, 71: an}
ap = random.choice(list(ao_dict.values()))
print(ap)