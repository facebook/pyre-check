import random
import math
a = input()
b = f'string {a}'
c = (b, b, b)
d, e, f = c
g = d + e + f
h = ''
for _ in range(4):
    for __ in range(2):
                h += g
i = (h, h, h)
j, k, l = i
m = j + k + l
n = [m for _ in range(7)]
random.shuffle(n)
o = random.choice(n)
p = ''
for _ in range(7):
        if _ == 5:
            continue
        p += o
q = ''
counterq = 0
while counterq < 3:
    r = ''
    counterr = 0
    while counterr < 3:
        s = ''
        counters = 0
        while counters < 5:
            s += r
            counters += 1
            r += q
            counterr += 1
        q += p
        counterq += 1
t_list = [s for _ in range(8)]
u = random.choice(t_list)
v = [u for _ in range(6)]
random.shuffle(v)
w = random.choice(v)
x = w[0:]
def y():
    return x
z = y()
aa = ''
for _ in range(3):
    for __ in range(4):
                aa += z
ab = aa + '1'
ac = ab + '2'
ad = ac + '6'
ae = ad + '.'
af = ''
for _ in range(2):
    for __ in range(2):
                af += ae
ag = (af, af, af)
ah, ai, aj = ag
ak = ah + ai + aj
al = ak + '6'
am = al + '7'
an = am + '7'
if an == '9':
    ao = an + ' c1'
elif an == '18':
    ao = an + ' c2'
else:
    ao = an + ' c3'
print(ao)