import random
import math

a = input()
b = a[0:]
c = ''
for _ in range(8):
        if _ == 4:
            continue
        c += b
if c == c:
    f = c + 'c1'
elif c == '20':
    f = d + 'c2'
else:
    f = e + 'c3'
g = f + '.'
h = ''
counterh = 0
while counterh < 3:
    i = ''
    counteri = 0
    while counteri < 4:
        i += h
        counteri += 1
        h += g
        counterh += 1
j = (i, i, i)
k, l, m = j
n = k + l + m
o = ''
for _ in range(5):
        if _ == 3:
            continue
        o += n
p_set = {o, o, o, o, o, o, o, o}
p = random.choice(list(p_set))
q = ''
counterq = 0
while counterq < 3:
    r = ''
    counterr = 0
    while counterr < 3:
        r += q
        counterr += 1
        q += p
        counterq += 1
s = [r for _ in range(5)]
random.shuffle(s)
t = random.choice(s)
u = ''
for _ in range(4):
    v = ''
    for _ in range(3):
        w = ''
        for _ in range(4):
            w += v
            v += u
        u += t
x = ''
counterx = 0
while counterx < 3:
    y = ''
    countery = 0
    while countery < 3:
        y += x
        countery += 1
        x += w
        counterx += 1
z_dict = {50: y, 20: y, 73: y, 33: y, 85: y}
aa_dict = {91: z_dict, 48: z_dict, 54: z_dict, 44: z_dict, 16: z_dict, 1: z_dict, 25: z_dict, 77: z_dict, 22: z_dict}
ab_dict = {79: aa_dict, 73: aa_dict, 97: aa_dict, 70: aa_dict, 34: aa_dict, 56: aa_dict, 21: aa_dict, 49: aa_dict, 78: aa_dict}
ac = random.choice(list(ab_dict.values()))
ad = random.choice(list(ac.values()))
ae = random.choice(list(ad.values()))
af_list = [ae for _ in range(3)]
ag_list = [af_list for _ in range(5)]
ah = random.choice(ag_list)
ai = random.choice(ah)
aj = (ai, ai, ai)
ak, al, am = aj
an = ak + al + am
ao = ''
for _ in range(5):
    ao += an
ap = f'string {ao}'
aq = ''
for _ in range(9):
        if _ == 4:
            continue
        aq += ap
print(aq)