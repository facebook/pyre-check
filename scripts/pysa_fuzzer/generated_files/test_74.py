import random
import math

a = input()
b = ''
for _ in range(3):
    for __ in range(5):
                b += a
c = [b for _ in range(5)]
random.shuffle(c)
d = random.choice(c)
e_dict = {79: d, 35: d, 39: d, 29: d, 24: d, 26: d, 6: d, 18: d, 38: d, 99: d}
f_dict = {53: e_dict, 7: e_dict, 56: e_dict}
g = random.choice(list(f_dict.values()))
h = random.choice(list(g.values()))
i = ''
for _ in range(5):
    for __ in range(5):
                i += h
j = i[0:]
k = j + '4'
l = k + '6'
m = l + '4'
n = (m, m, m)
o, p, q = n
r = o + p + q
s = r + '1'
t = s[0:]
u = t + '2'
v = u + '7'
w = v + '1'
if w == w:
    z = w + 'c1'
elif w == '18':
    z = x + 'c2'
else:
    z = y + 'c3'
aa = z[0:]
ab = ''
for _ in range(8):
        if _ == 3:
            break
        ab += aa
def ac():
    return ab
ad = ac()
ae = ''
for _ in range(10):
        if _ == 3:
            break
        ae += ad
af = ae + '.'
ag = (af, af, af)
ah, ai, aj = ag
ak = ah + ai + aj
al = ''
for _ in range(4):
    am = ''
    for _ in range(3):
        an = ''
        for _ in range(5):
            an += am
            am += al
        al += ak
print(an)