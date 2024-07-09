import random
import math

a = input()
b = [a for _ in range(7)]
random.shuffle(b)
c = random.choice(b)
d_set = {c, c, c, c}
d = random.choice(list(d_set))
e_set = {d, d, d, d, d, d}
e = random.choice(list(e_set))
def f():
    return e
def g():
    return f()
h = g()
i = [h for _ in range(10)]
random.shuffle(i)
j = random.choice(i)
k = ''
for _ in range(3):
    for __ in range(2):
                k += j
l = ''
for _ in range(4):
    for __ in range(2):
                l += k
m = [l for _ in range(5)]
random.shuffle(m)
n = random.choice(m)
o = n + '2'
p = o + '2'
q = p + '7'
r = f'string {q}'
def s():
    return r
t = s()
u = t[0:]
v = u[0:]
w = ''
for _ in range(6):
        if _ == 3:
            break
        w += v
x_set = {w, w, w, w, w, w, w, w}
x = random.choice(list(x_set))
y = x[0:]
z = y + '2'
aa = ''
for _ in range(4):
    for __ in range(5):
                aa += z
print(aa)