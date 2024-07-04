import random
import math
a = input()
b = ''
for _ in range(3):
    for __ in range(4):
                b += a
c = ''
for _ in range(5):
    d = ''
    for _ in range(5):
        d += c
        c += b
e = ''
for _ in range(8):
        if _ == 3:
            break
        e += d
f_set = {e, e, e, e, e, e, e}
f = random.choice(list(f_set))
if f == '1':
    g = f + ' c1'
elif f == '19':
    g = f + ' c2'
else:
    g = f + ' c3'
h = ''
for _ in range(2):
    i = ''
    for _ in range(4):
        j = ''
        for _ in range(5):
            j += i
            i += h
        h += g
k = (j, j, j)
l, m, n = k
o = l + m + n
p = ''
counterp = 0
while counterp < 2:
    q = ''
    counterq = 0
    while counterq < 4:
        r = ''
        counterr = 0
        while counterr < 2:
            r += q
            counterr += 1
            q += p
            counterq += 1
        p += o
        counterp += 1
s = ''
for _ in range(6):
        if _ == 3:
            break
        s += r
t_dict = {40: s, 52: s, 94: s, 99: s, 88: s, 51: s, 84: s, 40: s}
u = random.choice(list(t_dict.values()))
if u == '1':
    v = u + ' c1'
elif u == '18':
    v = u + ' c2'
else:
    v = u + ' c3'
w_set = {v, v, v, v, v, v, v}
w = random.choice(list(w_set))
x = w + '.'
y = x[0:]
if y == '9':
    z = y + ' c1'
elif y == '16':
    z = y + ' c2'
else:
    z = y + ' c3'
aa = f'string {z}'
ab = ''
for _ in range(6):
        if _ == 2:
            break
        ab += aa
ac_set = {ab, ab, ab, ab, ab, ab}
ac = random.choice(list(ac_set))
print(ac)