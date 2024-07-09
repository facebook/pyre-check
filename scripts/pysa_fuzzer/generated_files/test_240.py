import random
import math

a = input()
b = [a for _ in range(9)]
random.shuffle(b)
c = random.choice(b)
d = ''
for _ in range(5):
    for __ in range(3):
                d += c
e_list = [d for _ in range(3)]
f = random.choice(e_list)
g = ''
for _ in range(9):
        if _ == 4:
            break
        g += f
h = g + '8'
i = h + '2'
j = ''
for _ in range(5):
    k = ''
    for _ in range(4):
        k += j
        j += i
l_set = {k, k, k, k, k, k, k}
l = random.choice(list(l_set))
m = f'string {l}'
n = [m for _ in range(5)]
random.shuffle(n)
o = random.choice(n)
p = ''
for _ in range(4):
    for __ in range(4):
                p += o
q_list = [p for _ in range(5)]
r = random.choice(q_list)
if r == r:
    u = r + 'c1'
elif r == '12':
    u = s + 'c2'
else:
    u = t + 'c3'
v = u[0:]
w = ''
counterw = 0
while counterw < 3:
    w += v
    counterw += 1
x = ''
for _ in range(4):
    for __ in range(4):
                x += w
y = ''
for _ in range(9):
        if _ == 4:
            continue
        y += x
z = f'string {y}'
aa = z[0:]
print(aa)