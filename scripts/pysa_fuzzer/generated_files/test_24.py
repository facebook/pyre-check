import random
import math

a = input()
b = ''
for _ in range(3):
    c = ''
    for _ in range(5):
        c += b
        b += a
d = (c, c, c)
e, f, g = d
h = e + f + g
i_list = [h for _ in range(10)]
j = random.choice(i_list)
k = j + '.'
l = [k for _ in range(6)]
random.shuffle(l)
m = random.choice(l)
n = m + '.'
o = ''
for _ in range(3):
    p = ''
    for _ in range(3):
        q = ''
        for _ in range(3):
            q += p
            p += o
        o += n
r = q + '4'
def s():
    return r
def t():
    return s()
u = t()
v = ''
counterv = 0
while counterv < 4:
    w = ''
    counterw = 0
    while counterw < 4:
        w += v
        counterw += 1
        v += u
        counterv += 1
if w == w:
    z = w + 'c1'
elif w == '20':
    z = x + 'c2'
else:
    z = y + 'c3'
aa = z[0:]
ab = f'string {aa}'
def ac():
    return ab
def ad():
    return ac()
ae = ad()
af_dict = {55: ae, 57: ae, 7: ae, 72: ae, 51: ae, 60: ae, 92: ae}
ag_dict = {90: af_dict, 95: af_dict}
ah_dict = {63: ag_dict, 73: ag_dict, 51: ag_dict, 20: ag_dict}
ai = random.choice(list(ah_dict.values()))
aj = random.choice(list(ai.values()))
ak = random.choice(list(aj.values()))
al_dict = {23: ak, 1: ak, 24: ak, 14: ak, 86: ak, 73: ak, 75: ak}
am = random.choice(list(al_dict.values()))
an = am + '6'
ao = an + '1'
ap = ao + '1'
aq = f'string {ap}'
print(aq)