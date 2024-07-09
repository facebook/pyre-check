import random
import math

a = input()
b = ''
for _ in range(9):
        if _ == 3:
            continue
        b += a
c = b[0:]
d = c + '1'
e = d + '4'
f = e + '3'
g = f + '1'
h = g + '1'
i_dict = {71: h, 50: h, 91: h, 19: h, 15: h, 2: h, 8: h, 81: h, 63: h}
j_dict = {21: i_dict, 27: i_dict, 75: i_dict, 44: i_dict, 2: i_dict, 68: i_dict, 69: i_dict}
k_dict = {89: j_dict, 15: j_dict, 56: j_dict, 100: j_dict}
l = random.choice(list(k_dict.values()))
m = random.choice(list(l.values()))
n = random.choice(list(m.values()))
o = (n, n, n)
p, q, r = o
s = p + q + r
t = [s for _ in range(5)]
random.shuffle(t)
u = random.choice(t)
v_set = {u, u, u, u, u, u, u, u, u, u}
v = random.choice(list(v_set))
w = ''
for _ in range(3):
    x = ''
    for _ in range(2):
        y = ''
        for _ in range(2):
            y += x
            x += w
        w += v
z = (y, y, y)
aa, ab, ac = z
ad = aa + ab + ac
ae_set = {ad, ad, ad}
ae = random.choice(list(ae_set))
af = ''
for _ in range(5):
    for __ in range(4):
                af += ae
ag = ''
for _ in range(3):
    ag += af
ah = ag + '3'
ai = ah + '8'
aj = ''
counteraj = 0
while counteraj < 3:
    ak = ''
    counterak = 0
    while counterak < 4:
        ak += aj
        counterak += 1
        aj += ai
        counteraj += 1
al = ''
for _ in range(4):
    for __ in range(2):
                al += ak
am = ''
for _ in range(5):
    an = ''
    for _ in range(3):
        an += am
        am += al
ao = ''
for _ in range(5):
        if _ == 5:
            continue
        ao += an
print(ao)