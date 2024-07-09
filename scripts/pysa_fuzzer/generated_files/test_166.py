import random
import math

a = input()
b = a[0:]
c = b + '3'
d = c + '5'
e = d + '6'
f_list = [e for _ in range(9)]
g = random.choice(f_list)
h = [g for _ in range(10)]
random.shuffle(h)
i = random.choice(h)
j = [i for _ in range(7)]
random.shuffle(j)
k = random.choice(j)
l = k[0:]
m_set = {l, l, l, l, l, l, l, l, l, l}
m = random.choice(list(m_set))
n_set = {m, m, m, m, m, m, m, m, m, m}
n = random.choice(list(n_set))
o = [n for _ in range(9)]
random.shuffle(o)
p = random.choice(o)
q_set = {p, p, p, p, p, p, p}
q = random.choice(list(q_set))
r = f'string {q}'
s = r[0:]
t = [s for _ in range(10)]
random.shuffle(t)
u = random.choice(t)
v = u[0:]
w = ''
counterw = 0
while counterw < 3:
    x = ''
    counterx = 0
    while counterx < 5:
        x += w
        counterx += 1
        w += v
        counterw += 1
y = f'string {x}'
z = [y for _ in range(6)]
random.shuffle(z)
aa = random.choice(z)
ab = aa + '.'
print(ab)