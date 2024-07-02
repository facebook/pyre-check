import random
import math
a = input()
b_list = [a for _ in range(7)]
c_list = [b_list for _ in range(6)]
d = random.choice(c_list)
e = random.choice(d)
f = f'string {e}'
g = f'string {f}'
h = g + '.'
i = h[0:]
j_set = {i, i, i, i, i}
j = random.choice(list(j_set))
k = ''
for _ in range(10):
        if _ == 4:
            continue
        k += j
l = f'string {k}'
m_dict = {17: l, 100: l, 25: l}
n = random.choice(list(m_dict.values()))
o = (n, n, n)
p, q, r = o
s = p + q + r
t = ''
for _ in range(4):
    u = ''
    for _ in range(3):
        u += t
        t += s
v = ''
counterv = 0
while counterv < 3:
    w = ''
    counterw = 0
    while counterw < 4:
        w += v
        counterw += 1
        v += u
        counterv += 1
x_set = {w, w, w, w, w, w, w}
x = random.choice(list(x_set))
y_list = [x for _ in range(3)]
z_list = [y_list for _ in range(9)]
aa_list = [z_list for _ in range(10)]
ab = random.choice(aa_list)
ac = random.choice(ab)
ad = random.choice(ac)
ae = ad + '.'
af = ae + '6'
ag = [af for _ in range(6)]
random.shuffle(ag)
ah = random.choice(ag)
ai = [ah for _ in range(6)]
random.shuffle(ai)
aj = random.choice(ai)
print(aj)