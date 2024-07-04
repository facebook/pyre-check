import random
import math
a = input()
b = ''
counterb = 0
while counterb < 3:
    c = ''
    counterc = 0
    while counterc < 3:
        d = ''
        counterd = 0
        while counterd < 4:
            d += c
            counterd += 1
            c += b
            counterc += 1
        b += a
        counterb += 1
e = [d for _ in range(9)]
random.shuffle(e)
f = random.choice(e)
g = [f for _ in range(9)]
random.shuffle(g)
h = random.choice(g)
i_list = [h for _ in range(2)]
j_list = [i_list for _ in range(7)]
k = random.choice(j_list)
l = random.choice(k)
m = ''
for _ in range(2):
    m += l
n = m + '4'
o = ''
for _ in range(5):
    for __ in range(2):
                o += n
p = ''
counterp = 0
while counterp < 2:
    q = ''
    counterq = 0
    while counterq < 4:
        q += p
        counterq += 1
        p += o
        counterp += 1
r = ''
counterr = 0
while counterr < 3:
    s = ''
    counters = 0
    while counters < 2:
        s += r
        counters += 1
        r += q
        counterr += 1
t = ''
for _ in range(4):
    u = ''
    for _ in range(4):
        v = ''
        for _ in range(3):
            v += u
            u += t
        t += s
w = ''
for _ in range(2):
    for __ in range(2):
                w += v
x = w + '.'
y = ''
for _ in range(2):
    for __ in range(2):
                y += x
z = [y for _ in range(5)]
random.shuffle(z)
aa = random.choice(z)
if aa == '8':
    ab = aa + ' c1'
elif aa == '17':
    ab = aa + ' c2'
else:
    ab = aa + ' c3'
ac = ''
counterac = 0
while counterac < 5:
    ac += ab
    counterac += 1
ad = f'string {ac}'
ae_list = [ad for _ in range(4)]
af = random.choice(ae_list)
print(af)