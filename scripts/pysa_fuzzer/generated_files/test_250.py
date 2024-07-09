import random
import math

a = input()
b = a + '.'
c = b + '9'
d = c + '3'
if d == d:
    g = d + 'c1'
elif d == '20':
    g = e + 'c2'
else:
    g = f + 'c3'
h = ''
for _ in range(5):
    for __ in range(5):
                h += g
i = ''
for _ in range(9):
        if _ == 4:
            continue
        i += h
j = ''
counterj = 0
while counterj < 4:
    k = ''
    counterk = 0
    while counterk < 5:
        k += j
        counterk += 1
        j += i
        counterj += 1
l = k + '.'
m = l + '.'
n = ''
countern = 0
while countern < 3:
    o = ''
    countero = 0
    while countero < 5:
        p = ''
        counterp = 0
        while counterp < 3:
            p += o
            counterp += 1
            o += n
            countero += 1
        n += m
        countern += 1
q = ''
counterq = 0
while counterq < 5:
    r = ''
    counterr = 0
    while counterr < 2:
        s = ''
        counters = 0
        while counters < 5:
            s += r
            counters += 1
            r += q
            counterr += 1
        q += p
        counterq += 1
t = s[0:]
u = t[0:]
v = u + '2'
w = v + '1'
x = ''
for _ in range(2):
    for __ in range(5):
                x += w
y = ''
for _ in range(4):
    z = ''
    for _ in range(2):
        z += y
        y += x
aa = ''
counteraa = 0
while counteraa < 5:
    ab = ''
    counterab = 0
    while counterab < 3:
        ab += aa
        counterab += 1
        aa += z
        counteraa += 1
ac = ab + '6'
ad = ''
counterad = 0
while counterad < 3:
    ae = ''
    counterae = 0
    while counterae < 3:
        af = ''
        counteraf = 0
        while counteraf < 5:
            af += ae
            counteraf += 1
            ae += ad
            counterae += 1
        ad += ac
        counterad += 1
print(af)