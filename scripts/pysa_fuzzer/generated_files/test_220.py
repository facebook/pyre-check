import random
import math

a = input()
b = ''
for _ in range(5):
        if _ == 5:
            break
        b += a
c = b[0:]
d = ''
for _ in range(4):
    for __ in range(4):
                d += c
e = ''
for _ in range(2):
    f = ''
    for _ in range(5):
        g = ''
        for _ in range(2):
            g += f
            f += e
        e += d
h = f'string {g}'
i = [h for _ in range(7)]
random.shuffle(i)
j = random.choice(i)
k = ''
for _ in range(3):
    for __ in range(2):
                k += j
l = f'string {k}'
m_set = {l, l}
m = random.choice(list(m_set))
n = [m for _ in range(8)]
random.shuffle(n)
o = random.choice(n)
p = ''
for _ in range(3):
    p += o
q_list = [p for _ in range(9)]
r = random.choice(q_list)
s = r[0:]
t = s + '2'
u = f'string {t}'
v = u + '.'
w = [v for _ in range(5)]
random.shuffle(w)
x = random.choice(w)
y = (x, x, x)
z, aa, ab = y
ac = z + aa + ab
print(ac)