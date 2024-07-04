import random
import math
a = input()
if a == '8':
    b = a + ' c1'
elif a == '13':
    b = a + ' c2'
else:
    b = a + ' c3'
c_set = {b, b, b}
c = random.choice(list(c_set))
d = c + '.'
e = f'string {d}'
f_dict = {31: e, 8: e, 45: e}
g = random.choice(list(f_dict.values()))
h = g + '.'
i = ''
counteri = 0
while counteri < 2:
    j = ''
    counterj = 0
    while counterj < 3:
        j += i
        counterj += 1
        i += h
        counteri += 1
k = (j, j, j)
l, m, n = k
o = l + m + n
p = ''
counterp = 0
while counterp < 3:
    q = ''
    counterq = 0
    while counterq < 5:
        q += p
        counterq += 1
        p += o
        counterp += 1
r_set = {q, q, q, q, q, q, q}
r = random.choice(list(r_set))
s = r + '.'
t = ''
countert = 0
while countert < 4:
    u = ''
    counteru = 0
    while counteru < 4:
        u += t
        counteru += 1
        t += s
        countert += 1
v = [u for _ in range(5)]
random.shuffle(v)
w = random.choice(v)
x = ''
for _ in range(4):
    x += w
y = ''
for _ in range(4):
    for __ in range(5):
                y += x
z_set = {y, y, y, y, y, y, y, y}
z = random.choice(list(z_set))
aa = (z, z, z)
ab, ac, ad = aa
ae = ab + ac + ad
af = f'string {ae}'
print(af)