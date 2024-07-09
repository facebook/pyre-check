import random
import math

a = input()
b = a + '5'
c_dict = {51: b, 44: b, 29: b, 92: b, 49: b, 49: b, 10: b, 80: b, 56: b, 36: b}
d = random.choice(list(c_dict.values()))
e = f'string {d}'
def f():
    return e
def g():
    return f()
def h():
    return g()
i = h()
j = ''
for _ in range(2):
    for __ in range(4):
                j += i
k = j + '8'
l = k + '3'
m = l + '2'
n = m + '.'
o = f'string {n}'
p_list = [o for _ in range(2)]
q = random.choice(p_list)
r = ''
for _ in range(4):
    for __ in range(2):
                r += q
s = r + '.'
t = ''
for _ in range(3):
    for __ in range(4):
                t += s
u = ''
counteru = 0
while counteru < 5:
    v = ''
    counterv = 0
    while counterv < 2:
        v += u
        counterv += 1
        u += t
        counteru += 1
w = ''
for _ in range(3):
    x = ''
    for _ in range(4):
        x += w
        w += v
y = f'string {x}'
z = ''
for _ in range(7):
        if _ == 4:
            continue
        z += y
def aa():
    return z
def ab():
    return aa()
ac = ab()
ad = ''
for _ in range(6):
        if _ == 1:
            continue
        ad += ac
print(ad)