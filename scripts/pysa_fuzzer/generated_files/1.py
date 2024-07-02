import random
import math
a = input()
b = ''
for _ in range(5):
        if _ == 3:
            continue
        b += a
c = b + '4'
d = c + '7'
e = f'string {d}'
f = ''
for _ in range(5):
    g = ''
    for _ in range(2):
        g += f
        f += e
h = ''
for _ in range(5):
    for __ in range(2):
                h += g
i = (h, h, h)
j, k, l = i
m = j + k + l
n = m[0:]
o_set = {n, n, n, n, n, n, n, n}
o = random.choice(list(o_set))
p = ''
for _ in range(2):
    p += o
q = [p for _ in range(7)]
random.shuffle(q)
r = random.choice(q)
s = (r, r, r)
t, u, v = s
w = t + u + v
x = ''
for _ in range(5):
    for __ in range(2):
                x += w
y = x + '6'
z = y + '9'
aa = z + '4'
ab = [aa for _ in range(10)]
random.shuffle(ab)
ac = random.choice(ab)
if ac == '1':
    ad = ac + ' c1'
elif ac == '20':
    ad = ac + ' c2'
else:
    ad = ac + ' c3'
ae = ''
for _ in range(4):
    af = ''
    for _ in range(3):
        af += ae
        ae += ad
def ag():
    return af
def ah():
    return ag()
ai = ah()
aj = ''
for _ in range(8):
        if _ == 4:
            break
        aj += ai
ak = ''
for _ in range(5):
    ak += aj
al_set = {ak, ak, ak, ak, ak, ak, ak}
al = random.choice(list(al_set))
am = al + '.'
an_list = [am for _ in range(9)]
ao_list = [an_list for _ in range(3)]
ap = random.choice(ao_list)
aq = random.choice(ap)
ar_dict = {90: aq, 71: aq, 63: aq, 11: aq, 26: aq, 28: aq, 64: aq, 83: aq}
at = random.choice(list(ar_dict.values()))
au_list = [at for _ in range(7)]
av_list = [au_list for _ in range(7)]
aw = random.choice(av_list)
ax = random.choice(aw)
if ax == '8':
    ay = ax + ' c1'
elif ax == '11':
    ay = ax + ' c2'
else:
    ay = ax + ' c3'
def az():
    return ay
def ba():
    return az()
bb = ba()
bc = bb[0:]
bd = f'string {bc}'
print(bd)