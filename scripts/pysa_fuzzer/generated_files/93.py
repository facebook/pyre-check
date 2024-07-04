import random
import math
a = input()
b = ''
counterb = 0
while counterb < 4:
    b += a
    counterb += 1
c = ''
for _ in range(5):
        if _ == 3:
            continue
        c += b
d = [c for _ in range(9)]
random.shuffle(d)
e = random.choice(d)
f = e + '.'
g = [f for _ in range(6)]
random.shuffle(g)
h = random.choice(g)
i = [h for _ in range(10)]
random.shuffle(i)
j = random.choice(i)
if j == '5':
    k = j + ' c1'
elif j == '16':
    k = j + ' c2'
else:
    k = j + ' c3'
l = (k, k, k)
m, n, o = l
p = m + n + o
q = p[0:]
r = ''
for _ in range(3):
    s = ''
    for _ in range(3):
        t = ''
        for _ in range(5):
            t += s
            s += r
        r += q
u_dict = {15: t, 22: t, 26: t, 65: t}
v = random.choice(list(u_dict.values()))
w = ''
counterw = 0
while counterw < 2:
    w += v
    counterw += 1
x = w + '.'
y = x + '.'
def z():
    return y
def aa():
    return z()
def ab():
    return aa()
ac = ab()
ad = ''
for _ in range(3):
    for __ in range(5):
                ad += ac
ae = [ad for _ in range(6)]
random.shuffle(ae)
af = random.choice(ae)
ag = f'string {af}'
print(ag)