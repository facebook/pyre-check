import random
import math

a = input()
b = ''
for _ in range(6):
        if _ == 2:
            break
        b += a
c = ''
for _ in range(9):
        if _ == 5:
            continue
        c += b
d = ''
for _ in range(10):
        if _ == 2:
            break
        d += c
e = d[0:]
def f():
    return e
def g():
    return f()
def h():
    return g()
i = h()
j = i + '.'
k = [j for _ in range(9)]
random.shuffle(k)
l = random.choice(k)
m = ''
counterm = 0
while counterm < 2:
    m += l
    counterm += 1
n_list = [m for _ in range(7)]
o = random.choice(n_list)
p_set = {o, o, o, o, o, o, o}
p = random.choice(list(p_set))
q = f'string {p}'
r = q[0:]
s = r + '.'
t = s + '9'
u = t + '5'
v = u + '2'
w = ''
for _ in range(7):
        if _ == 4:
            break
        w += v
x_set = {w, w, w, w, w, w}
x = random.choice(list(x_set))
y = f'string {x}'
print(y)