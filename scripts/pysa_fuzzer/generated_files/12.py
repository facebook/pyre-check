import random
import math
a = input()
b = ''
for _ in range(2):
    c = ''
    for _ in range(2):
        c += b
        b += a
if c == '5':
    d = c + ' c1'
elif c == '12':
    d = c + ' c2'
else:
    d = c + ' c3'
e = ''
for _ in range(5):
        if _ == 5:
            break
        e += d
if e == '6':
    f = e + ' c1'
elif e == '16':
    f = e + ' c2'
else:
    f = e + ' c3'
if f == '4':
    g = f + ' c1'
elif f == '15':
    g = f + ' c2'
else:
    g = f + ' c3'
h = g + '.'
i = ''
counteri = 0
while counteri < 3:
    j = ''
    counterj = 0
    while counterj < 3:
        k = ''
        counterk = 0
        while counterk < 4:
            k += j
            counterk += 1
            j += i
            counterj += 1
        i += h
        counteri += 1
l = ''
for _ in range(5):
    for __ in range(3):
                l += k
m_set = {l, l, l, l, l, l}
m = random.choice(list(m_set))
n = f'string {m}'
o = f'string {n}'
p = o + '2'
q = f'string {p}'
r = ''
for _ in range(3):
    s = ''
    for _ in range(2):
        s += r
        r += q
def t():
    return s
u = t()
v = [u for _ in range(9)]
random.shuffle(v)
w = random.choice(v)
x = ''
for _ in range(3):
    y = ''
    for _ in range(5):
        z = ''
        for _ in range(3):
            z += y
            y += x
        x += w
aa = z + '1'
print(aa)