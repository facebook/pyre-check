import random
import math

a = input()
b = a[0:]
c = ''
for _ in range(3):
    c += b
d_dict = {93: c, 47: c, 26: c, 23: c, 39: c}
e_dict = {1: d_dict, 16: d_dict, 46: d_dict, 28: d_dict, 46: d_dict}
f = random.choice(list(e_dict.values()))
g = random.choice(list(f.values()))
h = g[0:]
i = (h, h, h)
j, k, l = i
m = j + k + l
n = (m, m, m)
o, p, q = n
r = o + p + q
s = ''
for _ in range(5):
    t = ''
    for _ in range(3):
        u = ''
        for _ in range(4):
            u += t
            t += s
        s += r
v = [u for _ in range(10)]
random.shuffle(v)
w = random.choice(v)
x = ''
counterx = 0
while counterx < 2:
    y = ''
    countery = 0
    while countery < 2:
        y += x
        countery += 1
        x += w
        counterx += 1
z = y + '.'
aa = f'string {z}'
ab = ''
for _ in range(3):
    for __ in range(3):
                ab += aa
ac = ab[0:]
ad = ''
for _ in range(4):
    ae = ''
    for _ in range(2):
        af = ''
        for _ in range(2):
            af += ae
            ae += ad
        ad += ac
ag = af[0:]
ah = ''
for _ in range(4):
    ai = ''
    for _ in range(3):
        aj = ''
        for _ in range(3):
            aj += ai
            ai += ah
        ah += ag
ak_list = [aj for _ in range(5)]
al_list = [ak_list for _ in range(2)]
am = random.choice(al_list)
an = random.choice(am)
ao_set = {an, an, an, an, an}
ao = random.choice(list(ao_set))
print(ao)