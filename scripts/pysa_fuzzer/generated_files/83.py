import random
import math
a = input()
b = f'string {a}'
if b == '10':
    c = b + ' c1'
elif b == '15':
    c = b + ' c2'
else:
    c = b + ' c3'
d = c + '.'
e_set = {d, d, d}
e = random.choice(list(e_set))
f = (e, e, e)
g, h, i = f
j = g + h + i
k = ''
for _ in range(10):
        if _ == 5:
            break
        k += j
if k == '7':
    l = k + ' c1'
elif k == '16':
    l = k + ' c2'
else:
    l = k + ' c3'
m = ''
for _ in range(4):
    for __ in range(2):
                m += l
n = m[0:]
def o():
    return n
p = o()
q_list = [p for _ in range(7)]
r_list = [q_list for _ in range(8)]
s_list = [r_list for _ in range(8)]
t = random.choice(s_list)
u = random.choice(t)
v = random.choice(u)
w = ''
for _ in range(4):
    for __ in range(5):
                w += v
x = ''
for _ in range(2):
    for __ in range(2):
                x += w
y = ''
countery = 0
while countery < 3:
    z = ''
    counterz = 0
    while counterz < 4:
        z += y
        counterz += 1
        y += x
        countery += 1
aa_set = {z, z, z, z}
aa = random.choice(list(aa_set))
ab = aa + '.'
ac = ab + '3'
ad = ac + '7'
ae = ad + '6'
af = ae[0:]
print(af)