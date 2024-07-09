import random
import math

a = input()
b = ''
for _ in range(8):
        if _ == 4:
            continue
        b += a
c = ''
for _ in range(10):
        if _ == 4:
            break
        c += b
d = ''
for _ in range(6):
        if _ == 4:
            continue
        d += c
e = d + '6'
f = e + '3'
g = f + '5'
h = (g, g, g)
i, j, k = h
l = i + j + k
m = f'string {l}'
n = m + '.'
o = n[0:]
p = o + '2'
q = ''
counterq = 0
while counterq < 4:
    r = ''
    counterr = 0
    while counterr < 3:
        s = ''
        counters = 0
        while counters < 3:
            s += r
            counters += 1
            r += q
            counterr += 1
        q += p
        counterq += 1
t_list = [s for _ in range(6)]
u_list = [t_list for _ in range(5)]
v_list = [u_list for _ in range(5)]
w = random.choice(v_list)
x = random.choice(w)
y = random.choice(x)
z = y + '.'
def aa():
    return z
def ab():
    return aa()
ac = ab()
ad = f'string {ac}'
ae = ad[0:]
af = f'string {ae}'
if af == af:
    ai = af + 'c1'
elif af == '20':
    ai = ag + 'c2'
else:
    ai = ah + 'c3'
if ai == ai:
    al = ai + 'c1'
elif ai == '16':
    al = aj + 'c2'
else:
    al = ak + 'c3'
print(al)