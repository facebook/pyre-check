import random
import math
a = input()
b_set = {a, a, a, a, a, a, a}
b = random.choice(list(b_set))
c = ''
for _ in range(2):
    c += b
d = ''
for _ in range(6):
        if _ == 1:
            break
        d += c
e_set = {d, d, d, d, d, d, d}
e = random.choice(list(e_set))
f = [e for _ in range(9)]
random.shuffle(f)
g = random.choice(f)
if g == '9':
    h = g + ' c1'
elif g == '16':
    h = g + ' c2'
else:
    h = g + ' c3'
i = (h, h, h)
j, k, l = i
m = j + k + l
n = m[0:]
def o():
    return n
p = o()
q = p + '5'
r = f'string {q}'
s = r[0:]
t = ''
for _ in range(3):
    u = ''
    for _ in range(2):
        u += t
        t += s
v = u[0:]
if v == '7':
    w = v + ' c1'
elif v == '17':
    w = v + ' c2'
else:
    w = v + ' c3'
x = ''
for _ in range(8):
        if _ == 1:
            continue
        x += w
y = x + '1'
z = y + '7'
aa = ''
for _ in range(6):
        if _ == 4:
            break
        aa += z
print(aa)