import random
import math

a = input()
def b():
    return a
def c():
    return b()
d = c()
e = d + '5'
f = e + '9'
g = f + '9'
h = ''
for _ in range(5):
        if _ == 2:
            continue
        h += g
i = h[0:]
j = f'string {i}'
k = ''
for _ in range(6):
        if _ == 4:
            continue
        k += j
l = k + '.'
m = l + '.'
n_set = {m, m, m, m, m, m, m, m}
n = random.choice(list(n_set))
o = n[0:]
p = o + '3'
q = p + '9'
r_set = {q, q, q, q, q}
r = random.choice(list(r_set))
s = r + '.'
t = [s for _ in range(6)]
random.shuffle(t)
u = random.choice(t)
v_set = {u, u, u}
v = random.choice(list(v_set))
w = [v for _ in range(10)]
random.shuffle(w)
x = random.choice(w)
y = ''
for _ in range(4):
    y += x
z = f'string {y}'
print(z)