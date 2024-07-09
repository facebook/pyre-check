import random
import math

a = input()
b = a + '1'
c = b + '4'
d = c[0:]
e = d + '9'
f = e + '7'
g = f + '1'
h = [g for _ in range(7)]
random.shuffle(h)
i = random.choice(h)
j = i + '.'
k = j[0:]
l = k[0:]
m = [l for _ in range(8)]
random.shuffle(m)
n = random.choice(m)
o = n + '.'
p = ''
for _ in range(5):
        if _ == 2:
            break
        p += o
q = p[0:]
r = ''
counterr = 0
while counterr < 2:
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
u = (t, t, t)
v, w, x = u
y = v + w + x
z = f'string {y}'
aa_list = [z for _ in range(6)]
ab = random.choice(aa_list)
ac = ''
for _ in range(6):
        if _ == 3:
            continue
        ac += ab
ad = ''
for _ in range(5):
    for __ in range(5):
                ad += ac
ae = ad + '1'
af = ae + '4'
print(af)