import random
import math
a = input()
b = [a for _ in range(9)]
random.shuffle(b)
c = random.choice(b)
d = f'string {c}'
e = f'string {d}'
f_dict = {78: e, 32: e, 99: e, 38: e, 36: e, 76: e, 75: e, 64: e}
g = random.choice(list(f_dict.values()))
h = ''
counterh = 0
while counterh < 3:
    i = ''
    counteri = 0
    while counteri < 4:
        j = ''
        counterj = 0
        while counterj < 4:
            j += i
            counterj += 1
            i += h
            counteri += 1
        h += g
        counterh += 1
k = ''
for _ in range(3):
    for __ in range(2):
                k += j
if k == '4':
    l = k + ' c1'
elif k == '18':
    l = k + ' c2'
else:
    l = k + ' c3'
def m():
    return l
def n():
    return m()
o = n()
p = ''
for _ in range(5):
    for __ in range(2):
                p += o
q = p + '1'
r = q + '6'
s = ''
counters = 0
while counters < 2:
    t = ''
    countert = 0
    while countert < 5:
        t += s
        countert += 1
        s += r
        counters += 1
u = ''
counteru = 0
while counteru < 4:
    u += t
    counteru += 1
v = ''
for _ in range(4):
    for __ in range(3):
                v += u
w = ''
counterw = 0
while counterw < 2:
    x = ''
    counterx = 0
    while counterx < 4:
        y = ''
        countery = 0
        while countery < 5:
            y += x
            countery += 1
            x += w
            counterx += 1
        w += v
        counterw += 1
z = y[0:]
aa_dict = {16: z, 41: z, 47: z, 84: z, 88: z, 80: z, 71: z, 25: z, 84: z}
ab_dict = {25: aa_dict, 1: aa_dict, 81: aa_dict, 78: aa_dict, 71: aa_dict, 84: aa_dict}
ac = random.choice(list(ab_dict.values()))
ad = random.choice(list(ac.values()))
ae_dict = {47: ad, 57: ad, 44: ad, 35: ad, 56: ad, 67: ad, 75: ad, 61: ad, 92: ad}
af = random.choice(list(ae_dict.values()))
def ag():
    return af
ah = ag()
print(ah)