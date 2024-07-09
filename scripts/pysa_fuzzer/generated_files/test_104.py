import random
import math

a = input()
b_list = [a for _ in range(6)]
c = random.choice(b_list)
d = [c for _ in range(10)]
random.shuffle(d)
e = random.choice(d)
f = ''
for _ in range(3):
    for __ in range(3):
                f += e
g = (f, f, f)
h, i, j = g
k = h + i + j
l = (k, k, k)
m, n, o = l
p = m + n + o
q = ''
counterq = 0
while counterq < 5:
    q += p
    counterq += 1
r = ''
for _ in range(8):
        if _ == 3:
            continue
        r += q
s = f'string {r}'
t = ''
for _ in range(10):
        if _ == 5:
            break
        t += s
u = t + '3'
v = f'string {u}'
w = f'string {v}'
x_set = {w, w, w, w, w, w, w, w, w, w}
x = random.choice(list(x_set))
y = x + '4'
z = ''
for _ in range(2):
    aa = ''
    for _ in range(5):
        ab = ''
        for _ in range(4):
            ab += aa
            aa += z
        z += y
ac_set = {ab, ab, ab, ab, ab, ab, ab}
ac = random.choice(list(ac_set))
ad = ac[0:]
ae = ''
for _ in range(4):
    for __ in range(4):
                ae += ad
print(ae)