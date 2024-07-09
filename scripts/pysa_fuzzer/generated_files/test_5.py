import random
import math

a = input()
b = f'string {a}'
c = [b for _ in range(10)]
random.shuffle(c)
d = random.choice(c)
e = ''
for _ in range(2):
    for __ in range(2):
                e += d
f = e + '2'
g = f + '4'
h = (g, g, g)
i, j, k = h
l = i + j + k
m = ''
counterm = 0
while counterm < 3:
    m += l
    counterm += 1
n = m + '1'
o = ''
for _ in range(3):
    p = ''
    for _ in range(3):
        q = ''
        for _ in range(2):
            q += p
            p += o
        o += n
r = (q, q, q)
s, t, u = r
v = s + t + u
w = v[0:]
x_dict = {78: w, 58: w, 78: w}
y = random.choice(list(x_dict.values()))
z_dict = {63: y, 11: y, 95: y, 85: y, 30: y, 45: y}
aa_dict = {79: z_dict, 36: z_dict, 59: z_dict, 98: z_dict, 85: z_dict, 75: z_dict}
ab_dict = {35: aa_dict, 60: aa_dict}
ac = random.choice(list(ab_dict.values()))
ad = random.choice(list(ac.values()))
ae = random.choice(list(ad.values()))
af = ae + '1'
ag_list = [af for _ in range(4)]
ah_list = [ag_list for _ in range(3)]
ai = random.choice(ah_list)
aj = random.choice(ai)
ak = ''
counterak = 0
while counterak < 2:
    al = ''
    counteral = 0
    while counteral < 2:
        al += ak
        counteral += 1
        ak += aj
        counterak += 1
am = (al, al, al)
an, ao, ap = am
aq = an + ao + ap
if aq == aq:
    au = aq + 'c1'
elif aq == '14':
    au = ar + 'c2'
else:
    au = at + 'c3'
av = au[0:]
print(av)