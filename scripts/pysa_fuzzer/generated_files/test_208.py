import random
import math

a = input()
b_dict = {67: a, 50: a, 69: a, 10: a, 80: a, 75: a, 52: a, 85: a, 81: a}
c_dict = {70: b_dict, 1: b_dict, 100: b_dict, 76: b_dict, 80: b_dict, 32: b_dict, 3: b_dict, 49: b_dict, 28: b_dict, 7: b_dict}
d = random.choice(list(c_dict.values()))
e = random.choice(list(d.values()))
f = e + '3'
g = f + '4'
h = g + '1'
i = (h, h, h)
j, k, l = i
m = j + k + l
n = ''
for _ in range(4):
    for __ in range(4):
                n += m
o = n + '.'
p = (o, o, o)
q, r, s = p
t = q + r + s
u = (t, t, t)
v, w, x = u
y = v + w + x
z_dict = {64: y, 42: y, 46: y}
aa = random.choice(list(z_dict.values()))
ab = (aa, aa, aa)
ac, ad, ae = ab
af = ac + ad + ae
ag = [af for _ in range(7)]
random.shuffle(ag)
ah = random.choice(ag)
ai = ah + '4'
aj = ai + '9'
ak = ''
for _ in range(3):
    for __ in range(4):
                ak += aj
al = ''
for _ in range(2):
    for __ in range(2):
                al += ak
am = al + '.'
def an():
    return am
def ao():
    return an()
def ap():
    return ao()
aq = ap()
ar = aq + '4'
at = ar + '5'
au = at + '8'
av_dict = {71: au, 23: au, 58: au, 54: au, 88: au}
aw_dict = {28: av_dict, 33: av_dict, 52: av_dict, 17: av_dict}
ax = random.choice(list(aw_dict.values()))
ay = random.choice(list(ax.values()))
az = ay + '2'
ba = az + '9'
bb = ba + '3'
print(bb)