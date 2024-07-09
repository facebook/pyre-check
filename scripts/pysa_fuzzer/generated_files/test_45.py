import random
import math

a = input()
b = ''
for _ in range(7):
        if _ == 4:
            continue
        b += a
c = (b, b, b)
d, e, f = c
g = d + e + f
h = (g, g, g)
i, j, k = h
l = i + j + k
m = ''
for _ in range(3):
    n = ''
    for _ in range(4):
        o = ''
        for _ in range(4):
            o += n
            n += m
        m += l
p = ''
for _ in range(5):
    q = ''
    for _ in range(5):
        r = ''
        for _ in range(4):
            r += q
            q += p
        p += o
s = ''
for _ in range(2):
    for __ in range(3):
                s += r
t = s[0:]
u = (t, t, t)
v, w, x = u
y = v + w + x
z = ''
for _ in range(4):
    aa = ''
    for _ in range(2):
        aa += z
        z += y
ab = aa + '.'
ac_list = [ab for _ in range(2)]
ad = random.choice(ac_list)
ae_dict = {29: ad, 23: ad}
af = random.choice(list(ae_dict.values()))
ag = [af for _ in range(8)]
random.shuffle(ag)
ah = random.choice(ag)
if ah == ah:
    ak = ah + 'c1'
elif ah == '12':
    ak = ai + 'c2'
else:
    ak = aj + 'c3'
def al():
    return ak
def am():
    return al()
an = am()
ao_dict = {99: an, 25: an, 76: an}
ap = random.choice(list(ao_dict.values()))
aq = ap + '1'
ar = aq + '4'
at = ar + '8'
au = at + '9'
print(au)