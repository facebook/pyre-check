import random
import math
a = input()
b = ''
for _ in range(7):
        if _ == 3:
            continue
        b += a
c = ''
counterc = 0
while counterc < 3:
    c += b
    counterc += 1
d = c[0:]
e = ''
for _ in range(7):
        if _ == 4:
            continue
        e += d
f = e[0:]
g = f + '1'
h = g + '3'
i = h + '2'
j = ''
for _ in range(6):
        if _ == 4:
            break
        j += i
k = ''
counterk = 0
while counterk < 2:
    l = ''
    counterl = 0
    while counterl < 4:
        l += k
        counterl += 1
        k += j
        counterk += 1
m = l + '9'
n = ''
for _ in range(5):
        if _ == 2:
            continue
        n += m
o = [n for _ in range(6)]
random.shuffle(o)
p = random.choice(o)
q = p[0:]
r = q + '7'
s = r + '9'
t = s + '6'
u = [t for _ in range(5)]
random.shuffle(u)
v = random.choice(u)
w = (v, v, v)
x, y, z = w
aa = x + y + z
ab = ''
counterab = 0
while counterab < 4:
    ac = ''
    counterac = 0
    while counterac < 5:
        ad = ''
        counterad = 0
        while counterad < 4:
            ad += ac
            counterad += 1
            ac += ab
            counterac += 1
        ab += aa
        counterab += 1
ae = ''
for _ in range(5):
        if _ == 4:
            continue
        ae += ad
af = [ae for _ in range(10)]
random.shuffle(af)
ag = random.choice(af)
print(ag)