import random
import math

a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g = (f, f, f)
h, i, j = g
k = h + i + j
l = ''
for _ in range(2):
    m = ''
    for _ in range(3):
        m += l
        l += k
n = ''
for _ in range(9):
        if _ == 5:
            continue
        n += m
o = ''
for _ in range(5):
    o += n
p_list = [o for _ in range(2)]
q_list = [p_list for _ in range(3)]
r = random.choice(q_list)
s = random.choice(r)
t = (s, s, s)
u, v, w = t
x = u + v + w
if x == x:
    aa = x + 'c1'
elif x == '19':
    aa = y + 'c2'
else:
    aa = z + 'c3'
ab = [aa for _ in range(5)]
random.shuffle(ab)
ac = random.choice(ab)
ad = [ac for _ in range(10)]
random.shuffle(ad)
ae = random.choice(ad)
af = ''
for _ in range(4):
    ag = ''
    for _ in range(4):
        ag += af
        af += ae
ah = ''
for _ in range(2):
    ah += ag
ai = ah[0:]
def aj():
    return ai
def ak():
    return aj()
al = ak()
am = ''
for _ in range(5):
        if _ == 1:
            continue
        am += al
an_dict = {40: am, 39: am, 51: am, 19: am}
ao_dict = {82: an_dict, 76: an_dict, 49: an_dict}
ap = random.choice(list(ao_dict.values()))
aq = random.choice(list(ap.values()))
if aq == aq:
    au = aq + 'c1'
elif aq == '15':
    au = ar + 'c2'
else:
    au = at + 'c3'
av = (au, au, au)
aw, ax, ay = av
az = aw + ax + ay
print(az)