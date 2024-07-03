import random
import math
a = input()
b = f'string {a}'
c = f'string {b}'
d = ''
for _ in range(3):
    e = ''
    for _ in range(5):
        e += d
        d += c
f = ''
for _ in range(4):
    for __ in range(2):
                f += e
g = ''
for _ in range(4):
    for __ in range(2):
                g += f
h = ''
counterh = 0
while counterh < 3:
    i = ''
    counteri = 0
    while counteri < 5:
        j = ''
        counterj = 0
        while counterj < 2:
            j += i
            counterj += 1
            i += h
            counteri += 1
        h += g
        counterh += 1
k = j[0:]
l = [k for _ in range(6)]
random.shuffle(l)
m = random.choice(l)
n_dict = {1: m, 49: m, 75: m, 3: m, 38: m}
o = random.choice(list(n_dict.values()))
p = ''
for _ in range(2):
    for __ in range(4):
                p += o
q = (p, p, p)
r, s, t = q
u = r + s + t
def v():
    return u
def w():
    return v()
x = w()
y_list = [x for _ in range(5)]
z = random.choice(y_list)
aa = z + '.'
def ab():
    return aa
ac = ab()
ad = ''
for _ in range(3):
    for __ in range(2):
                ad += ac
ae = [ad for _ in range(6)]
random.shuffle(ae)
af = random.choice(ae)
ag = ''
for _ in range(3):
    for __ in range(2):
                ag += af
print(ag)