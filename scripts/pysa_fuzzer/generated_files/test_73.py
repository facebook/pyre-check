import random
import math

a = input()
b = ''
counterb = 0
while counterb < 5:
    b += a
    counterb += 1
c_set = {b, b, b}
c = random.choice(list(c_set))
d = (c, c, c)
e, f, g = d
h = e + f + g
i = h[0:]
j = ''
for _ in range(2):
    for __ in range(2):
                j += i
k = f'string {j}'
l = (k, k, k)
m, n, o = l
p = m + n + o
q_list = [p for _ in range(9)]
r = random.choice(q_list)
s = [r for _ in range(6)]
random.shuffle(s)
t = random.choice(s)
u = ''
for _ in range(4):
    v = ''
    for _ in range(2):
        w = ''
        for _ in range(4):
            w += v
            v += u
        u += t
x = ''
for _ in range(3):
    y = ''
    for _ in range(5):
        y += x
        x += w
def z():
    return y
aa = z()
def ab():
    return aa
ac = ab()
ad = f'string {ac}'
ae = ad + '8'
af = ae + '9'
ag = af + '7'
ah = ag + '7'
ai = ''
for _ in range(3):
    for __ in range(2):
                ai += ah
aj_set = {ai, ai, ai, ai, ai, ai, ai, ai, ai, ai}
aj = random.choice(list(aj_set))
print(aj)