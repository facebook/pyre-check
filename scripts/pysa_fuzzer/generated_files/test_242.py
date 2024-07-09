import random
import math

a = input()
b_set = {a, a, a, a, a, a, a, a, a}
b = random.choice(list(b_set))
c = [b for _ in range(10)]
random.shuffle(c)
d = random.choice(c)
e = f'string {d}'
if e == e:
    h = e + 'c1'
elif e == '18':
    h = f + 'c2'
else:
    h = g + 'c3'
i = (h, h, h)
j, k, l = i
m = j + k + l
n = m + '6'
o = n + '5'
p = ''
counterp = 0
while counterp < 5:
    q = ''
    counterq = 0
    while counterq < 3:
        q += p
        counterq += 1
        p += o
        counterp += 1
r = q[0:]
s = f'string {r}'
t = ''
countert = 0
while countert < 3:
    u = ''
    counteru = 0
    while counteru < 2:
        v = ''
        counterv = 0
        while counterv < 3:
            v += u
            counterv += 1
            u += t
            counteru += 1
        t += s
        countert += 1
def w():
    return v
x = w()
y = (x, x, x)
z, aa, ab = y
ac = z + aa + ab
ad = ac + '.'
ae = f'string {ad}'
af = (ae, ae, ae)
ag, ah, ai = af
aj = ag + ah + ai
def ak():
    return aj
al = ak()
am = ''
for _ in range(3):
    for __ in range(4):
                am += al
an_dict = {17: am, 84: am, 88: am}
ao_dict = {49: an_dict, 10: an_dict, 28: an_dict, 42: an_dict, 6: an_dict}
ap_dict = {18: ao_dict, 86: ao_dict, 6: ao_dict, 15: ao_dict, 25: ao_dict, 59: ao_dict, 27: ao_dict, 40: ao_dict}
aq = random.choice(list(ap_dict.values()))
ar = random.choice(list(aq.values()))
at = random.choice(list(ar.values()))
print(at)