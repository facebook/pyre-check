import random
import math

a = input()
def b():
    return a
c = b()
d = ''
counterd = 0
while counterd < 5:
    d += c
    counterd += 1
e = d + '5'
f = f'string {e}'
def g():
    return f
def h():
    return g()
i = h()
j = [i for _ in range(10)]
random.shuffle(j)
k = random.choice(j)
l = ''
for _ in range(9):
        if _ == 5:
            continue
        l += k
m_dict = {79: l, 40: l, 95: l, 91: l, 100: l, 21: l, 82: l, 37: l}
n = random.choice(list(m_dict.values()))
o_set = {n, n, n, n, n, n, n, n, n, n}
o = random.choice(list(o_set))
p = o[0:]
q = f'string {p}'
r = ''
for _ in range(9):
        if _ == 4:
            continue
        r += q
s = ''
for _ in range(5):
        if _ == 4:
            continue
        s += r
t = s + '1'
u = t + '2'
v = f'string {u}'
w = v[0:]
x = ''
for _ in range(6):
        if _ == 5:
            continue
        x += w
y = x + '1'
z = y + '8'
print(z)