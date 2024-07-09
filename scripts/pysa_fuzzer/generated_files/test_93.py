import random
import math

a = input()
b = ''
for _ in range(3):
    for __ in range(2):
                b += a
c = b[0:]
d = ''
for _ in range(3):
    e = ''
    for _ in range(2):
        f = ''
        for _ in range(5):
            f += e
            e += d
        d += c
g = ''
for _ in range(5):
    for __ in range(4):
                g += f
h = ''
counterh = 0
while counterh < 3:
    h += g
    counterh += 1
i = ''
counteri = 0
while counteri < 4:
    j = ''
    counterj = 0
    while counterj < 3:
        j += i
        counterj += 1
        i += h
        counteri += 1
k = ''
for _ in range(3):
    for __ in range(3):
                k += j
l = [k for _ in range(8)]
random.shuffle(l)
m = random.choice(l)
n = m + '.'
o_list = [n for _ in range(3)]
p_list = [o_list for _ in range(6)]
q = random.choice(p_list)
r = random.choice(q)
s = r[0:]
if s == s:
    v = s + 'c1'
elif s == '19':
    v = t + 'c2'
else:
    v = u + 'c3'
w = ''
for _ in range(2):
    x = ''
    for _ in range(2):
        x += w
        w += v
y_dict = {44: x, 99: x, 19: x, 63: x, 34: x, 36: x}
z_dict = {89: y_dict, 54: y_dict, 34: y_dict, 96: y_dict, 56: y_dict, 98: y_dict, 19: y_dict}
aa_dict = {91: z_dict, 12: z_dict, 27: z_dict, 19: z_dict}
ab = random.choice(list(aa_dict.values()))
ac = random.choice(list(ab.values()))
ad = random.choice(list(ac.values()))
ae = [ad for _ in range(6)]
random.shuffle(ae)
af = random.choice(ae)
ag = af[0:]
ah = ''
for _ in range(5):
    ai = ''
    for _ in range(3):
        aj = ''
        for _ in range(5):
            aj += ai
            ai += ah
        ah += ag
ak = aj[0:]
print(ak)