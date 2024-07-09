import random
import math

a = input()
b = ''
for _ in range(3):
    c = ''
    for _ in range(3):
        d = ''
        for _ in range(4):
            d += c
            c += b
        b += a
e = ''
for _ in range(5):
    f = ''
    for _ in range(2):
        g = ''
        for _ in range(3):
            g += f
            f += e
        e += d
h = ''
for _ in range(7):
        if _ == 4:
            continue
        h += g
i = [h for _ in range(8)]
random.shuffle(i)
j = random.choice(i)
k = j[0:]
l = k[0:]
m = l[0:]
n = ''
for _ in range(4):
    for __ in range(5):
                n += m
o = ''
for _ in range(5):
    p = ''
    for _ in range(5):
        p += o
        o += n
q = f'string {p}'
r = ''
for _ in range(5):
    for __ in range(4):
                r += q
s = [r for _ in range(8)]
random.shuffle(s)
t = random.choice(s)
u = (t, t, t)
v, w, x = u
y = v + w + x
z = y + '.'
aa_dict = {42: z, 47: z, 27: z, 91: z, 75: z, 59: z, 82: z}
ab = random.choice(list(aa_dict.values()))
ac = f'string {ab}'
ad = ''
for _ in range(5):
    ad += ac
ae_set = {ad, ad, ad}
ae = random.choice(list(ae_set))
print(ae)