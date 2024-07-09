import random
import math

a = input()
b = a + '.'
c_list = [b for _ in range(4)]
d_list = [c_list for _ in range(6)]
e = random.choice(d_list)
f = random.choice(e)
g = ''
counterg = 0
while counterg < 3:
    h = ''
    counterh = 0
    while counterh < 5:
        i = ''
        counteri = 0
        while counteri < 5:
            i += h
            counteri += 1
            h += g
            counterh += 1
        g += f
        counterg += 1
j = ''
for _ in range(10):
        if _ == 1:
            continue
        j += i
k = ''
for _ in range(10):
        if _ == 5:
            break
        k += j
if k == k:
    n = k + 'c1'
elif k == '11':
    n = l + 'c2'
else:
    n = m + 'c3'
o = [n for _ in range(9)]
random.shuffle(o)
p = random.choice(o)
q = [p for _ in range(10)]
random.shuffle(q)
r = random.choice(q)
s = ''
for _ in range(4):
    for __ in range(2):
                s += r
t_list = [s for _ in range(2)]
u = random.choice(t_list)
v = u[0:]
w = f'string {v}'
x_set = {w, w}
x = random.choice(list(x_set))
y = ''
countery = 0
while countery < 2:
    y += x
    countery += 1
z = y + '7'
aa = (z, z, z)
ab, ac, ad = aa
ae = ab + ac + ad
af = ''
for _ in range(4):
    for __ in range(3):
                af += ae
ag = f'string {af}'
print(ag)