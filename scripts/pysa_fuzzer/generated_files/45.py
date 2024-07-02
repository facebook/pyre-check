import random
import math
a = input()
b = ''
counterb = 0
while counterb < 5:
    b += a
    counterb += 1
c_dict = {19: b, 99: b, 61: b, 77: b, 53: b}
d_dict = {58: c_dict, 84: c_dict, 69: c_dict, 9: c_dict, 71: c_dict, 47: c_dict, 79: c_dict, 89: c_dict, 77: c_dict, 62: c_dict}
e_dict = {32: d_dict, 95: d_dict, 20: d_dict}
f = random.choice(list(e_dict.values()))
g = random.choice(list(f.values()))
h = random.choice(list(g.values()))
i = ''
counteri = 0
while counteri < 5:
    i += h
    counteri += 1
j = i + '2'
k = j + '4'
l = k + '7'
m = l[0:]
n = m[0:]
o = n + '.'
p = o + '.'
q = ''
counterq = 0
while counterq < 4:
    q += p
    counterq += 1
r = ''
counterr = 0
while counterr < 3:
    s = ''
    counters = 0
    while counters < 5:
        t = ''
        countert = 0
        while countert < 4:
            t += s
            countert += 1
            s += r
            counters += 1
        r += q
        counterr += 1
u = t + '.'
v = ''
counterv = 0
while counterv < 5:
    w = ''
    counterw = 0
    while counterw < 2:
        x = ''
        counterx = 0
        while counterx < 4:
            x += w
            counterx += 1
            w += v
            counterw += 1
        v += u
        counterv += 1
y = [x for _ in range(10)]
random.shuffle(y)
z = random.choice(y)
aa_dict = {48: z, 3: z, 13: z, 100: z, 32: z}
ab = random.choice(list(aa_dict.values()))
ac = ab[0:]
ad = f'string {ac}'
ae = (ad, ad, ad)
af, ag, ah = ae
ai = af + ag + ah
aj = ai + '.'
print(aj)