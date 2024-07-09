import random
import math

a = input()
b = f'string {a}'
c = f'string {b}'
d = (c, c, c)
e, f, g = d
h = e + f + g
i = f'string {h}'
j = [i for _ in range(10)]
random.shuffle(j)
k = random.choice(j)
l = ''
counterl = 0
while counterl < 2:
    m = ''
    counterm = 0
    while counterm < 2:
        n = ''
        countern = 0
        while countern < 3:
            n += m
            countern += 1
            m += l
            counterm += 1
        l += k
        counterl += 1
o = ''
for _ in range(4):
    o += n
p = f'string {o}'
q = ''
for _ in range(7):
        if _ == 4:
            break
        q += p
r = ''
for _ in range(5):
    s = ''
    for _ in range(2):
        t = ''
        for _ in range(2):
            t += s
            s += r
        r += q
u = [t for _ in range(7)]
random.shuffle(u)
v = random.choice(u)
w = f'string {v}'
x = ''
for _ in range(8):
        if _ == 4:
            continue
        x += w
y_set = {x, x, x, x, x, x, x, x, x, x}
y = random.choice(list(y_set))
z = ''
for _ in range(4):
    aa = ''
    for _ in range(3):
        aa += z
        z += y
ab = [aa for _ in range(9)]
random.shuffle(ab)
ac = random.choice(ab)
ad = ''
for _ in range(10):
        if _ == 1:
            break
        ad += ac
ae = ad[0:]
print(ae)