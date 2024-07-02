import random
import math
a = input()
b = ''
for _ in range(5):
        if _ == 4:
            break
        b += a
c = [b for _ in range(10)]
random.shuffle(c)
d = random.choice(c)
if d == '2':
    e = d + ' c1'
elif d == '13':
    e = d + ' c2'
else:
    e = d + ' c3'
f = ''
for _ in range(3):
    for __ in range(5):
                f += e
g = ''
for _ in range(2):
    for __ in range(2):
                g += f
h = [g for _ in range(9)]
random.shuffle(h)
i = random.choice(h)
j = ''
counterj = 0
while counterj < 2:
    k = ''
    counterk = 0
    while counterk < 3:
        l = ''
        counterl = 0
        while counterl < 5:
            l += k
            counterl += 1
            k += j
            counterk += 1
        j += i
        counterj += 1
m = ''
for _ in range(10):
        if _ == 1:
            break
        m += l
n_list = [m for _ in range(3)]
o_list = [n_list for _ in range(4)]
p = random.choice(o_list)
q = random.choice(p)
r = ''
counterr = 0
while counterr < 2:
    s = ''
    counters = 0
    while counters < 4:
        t = ''
        countert = 0
        while countert < 3:
            t += s
            countert += 1
            s += r
            counters += 1
        r += q
        counterr += 1
u_list = [t for _ in range(7)]
v = random.choice(u_list)
w = f'string {v}'
x = ''
for _ in range(5):
    x += w
y = x + '7'
z = y + '9'
aa = z + '3'
ab = aa + '1'
ac = ''
for _ in range(4):
    for __ in range(5):
                ac += ab
ad = ''
for _ in range(9):
        if _ == 2:
            continue
        ad += ac
ae = ad + '.'
print(ae)