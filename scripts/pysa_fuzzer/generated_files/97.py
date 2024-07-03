import random
import math
a = input()
b = a + '.'
c = ''
for _ in range(5):
        if _ == 4:
            break
        c += b
d = (c, c, c)
e, f, g = d
h = e + f + g
i = [h for _ in range(9)]
random.shuffle(i)
j = random.choice(i)
k = j + '.'
l = k[0:]
m = l + '.'
n = [m for _ in range(7)]
random.shuffle(n)
o = random.choice(n)
p = ''
for _ in range(7):
        if _ == 2:
            break
        p += o
q = p + '.'
def r():
    return q
def s():
    return r()
def t():
    return s()
u = t()
v = u + '3'
w = v + '6'
x = ''
for _ in range(5):
    for __ in range(2):
                x += w
y = ''
for _ in range(4):
    for __ in range(4):
                y += x
z = f'string {y}'
if z == '5':
    aa = z + ' c1'
elif z == '11':
    aa = z + ' c2'
else:
    aa = z + ' c3'
ab = ''
for _ in range(3):
    ac = ''
    for _ in range(4):
        ac += ab
        ab += aa
ad = ac[0:]
print(ad)