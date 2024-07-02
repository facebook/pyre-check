import random
import math
a = input()
b = a[0:]
c = ''
for _ in range(8):
        if _ == 1:
            continue
        c += b
d = f'string {c}'
e = [d for _ in range(5)]
random.shuffle(e)
f = random.choice(e)
g = [f for _ in range(8)]
random.shuffle(g)
h = random.choice(g)
i = [h for _ in range(9)]
random.shuffle(i)
j = random.choice(i)
k = [j for _ in range(10)]
random.shuffle(k)
l = random.choice(k)
m = ''
for _ in range(2):
    n = ''
    for _ in range(4):
        n += m
        m += l
def o():
    return n
p = o()
q = [p for _ in range(10)]
random.shuffle(q)
r = random.choice(q)
s = f'string {r}'
t = ''
for _ in range(6):
        if _ == 5:
            continue
        t += s
u = f'string {t}'
v = ''
counterv = 0
while counterv < 2:
    v += u
    counterv += 1
if v == '6':
    w = v + ' c1'
elif v == '18':
    w = v + ' c2'
else:
    w = v + ' c3'
x = ''
for _ in range(5):
        if _ == 2:
            break
        x += w
y_dict = {39: x, 85: x, 22: x, 26: x}
z = random.choice(list(y_dict.values()))
aa = [z for _ in range(8)]
random.shuffle(aa)
ab = random.choice(aa)
print(ab)