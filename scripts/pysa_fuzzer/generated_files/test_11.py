import random
import math

a = input()
b = f'string {a}'
c_dict = {93: b, 21: b, 39: b}
d = random.choice(list(c_dict.values()))
e = ''
countere = 0
while countere < 3:
    f = ''
    counterf = 0
    while counterf < 3:
        g = ''
        counterg = 0
        while counterg < 2:
            g += f
            counterg += 1
            f += e
            counterf += 1
        e += d
        countere += 1
h = (g, g, g)
i, j, k = h
l = i + j + k
m = l + '.'
n = m + '.'
o = f'string {n}'
p = [o for _ in range(7)]
random.shuffle(p)
q = random.choice(p)
r = ''
counterr = 0
while counterr < 3:
    s = ''
    counters = 0
    while counters < 4:
        s += r
        counters += 1
        r += q
        counterr += 1
t = f'string {s}'
u = t[0:]
v = u + '8'
w = v + '8'
x = w + '1'
y = ''
for _ in range(4):
    z = ''
    for _ in range(3):
        z += y
        y += x
aa = z + '.'
ab = ''
for _ in range(9):
        if _ == 2:
            break
        ab += aa
ac = ''
for _ in range(4):
    ac += ab
if ac == ac:
    af = ac + 'c1'
elif ac == '19':
    af = ad + 'c2'
else:
    af = ae + 'c3'
ag = f'string {af}'
print(ag)