import random
import math

a = input()
b = a + '.'
c_list = [b for _ in range(4)]
d = random.choice(c_list)
def e():
    return d
def f():
    return e()
def g():
    return f()
h = g()
i = (h, h, h)
j, k, l = i
m = j + k + l
n = f'string {m}'
o = ''
for _ in range(3):
    for __ in range(5):
                o += n
p = ''
for _ in range(2):
    q = ''
    for _ in range(5):
        q += p
        p += o
r = (q, q, q)
s, t, u = r
v = s + t + u
w = v + '.'
x = ''
counterx = 0
while counterx < 4:
    x += w
    counterx += 1
y = x + '4'
z = (y, y, y)
aa, ab, ac = z
ad = aa + ab + ac
ae = f'string {ad}'
af = f'string {ae}'
ag = ''
for _ in range(9):
        if _ == 5:
            break
        ag += af
ah_list = [ag for _ in range(5)]
ai = random.choice(ah_list)
aj = ''
for _ in range(4):
    aj += ai
ak = aj + '9'
print(ak)