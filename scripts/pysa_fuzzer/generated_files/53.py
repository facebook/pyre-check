import random
import math
a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g_list = [f for _ in range(2)]
h = random.choice(g_list)
i = h[0:]
j = f'string {i}'
if j == '1':
    k = j + ' c1'
elif j == '19':
    k = j + ' c2'
else:
    k = j + ' c3'
l = f'string {k}'
m = ''
for _ in range(5):
    for __ in range(5):
                m += l
def n():
    return m
def o():
    return n()
p = o()
q = ''
for _ in range(2):
    for __ in range(2):
                q += p
r = q + '3'
s = r + '5'
t = s + '2'
u = t + '.'
v = u + '6'
w = v + '8'
x = w + '9'
y = f'string {x}'
z = y[0:]
aa = ''
for _ in range(3):
    for __ in range(5):
                aa += z
ab = ''
for _ in range(4):
    ab += aa
ac = (ab, ab, ab)
ad, ae, af = ac
ag = ad + ae + af
ah = ''
for _ in range(5):
    ai = ''
    for _ in range(3):
        ai += ah
        ah += ag
print(ai)