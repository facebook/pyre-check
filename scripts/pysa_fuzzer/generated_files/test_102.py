import random
import math

a = input()
b_set = {a, a}
b = random.choice(list(b_set))
c = ''
for _ in range(4):
    d = ''
    for _ in range(5):
        d += c
        c += b
e = (d, d, d)
f, g, h = e
i = f + g + h
j = ''
for _ in range(4):
    k = ''
    for _ in range(4):
        l = ''
        for _ in range(4):
            l += k
            k += j
        j += i
m = ''
counterm = 0
while counterm < 4:
    n = ''
    countern = 0
    while countern < 4:
        o = ''
        countero = 0
        while countero < 2:
            o += n
            countero += 1
            n += m
            countern += 1
        m += l
        counterm += 1
p = ''
for _ in range(5):
        if _ == 4:
            continue
        p += o
q = p + '.'
r_set = {q, q, q}
r = random.choice(list(r_set))
s = ''
counters = 0
while counters < 4:
    t = ''
    countert = 0
    while countert < 5:
        t += s
        countert += 1
        s += r
        counters += 1
def u():
    return t
v = u()
w = v + '.'
x = f'string {w}'
y = ''
for _ in range(2):
    for __ in range(3):
                y += x
z = y[0:]
aa = ''
for _ in range(5):
    for __ in range(5):
                aa += z
if aa == aa:
    ad = aa + 'c1'
elif aa == '19':
    ad = ab + 'c2'
else:
    ad = ac + 'c3'
ae = ''
for _ in range(4):
    for __ in range(2):
                ae += ad
if ae == ae:
    ah = ae + 'c1'
elif ae == '19':
    ah = af + 'c2'
else:
    ah = ag + 'c3'
print(ah)