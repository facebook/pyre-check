import random
import math

a = input()
if a == a:
    d = a + 'c1'
elif a == '13':
    d = b + 'c2'
else:
    d = c + 'c3'
e = ''
for _ in range(4):
    f = ''
    for _ in range(5):
        g = ''
        for _ in range(4):
            g += f
            f += e
        e += d
h = ''
for _ in range(5):
        if _ == 2:
            break
        h += g
i = ''
for _ in range(8):
        if _ == 5:
            continue
        i += h
j = ''
for _ in range(5):
    k = ''
    for _ in range(5):
        k += j
        j += i
l = ''
for _ in range(5):
    m = ''
    for _ in range(4):
        m += l
        l += k
n = ''
for _ in range(2):
    for __ in range(3):
                n += m
o = [n for _ in range(10)]
random.shuffle(o)
p = random.choice(o)
q = ''
for _ in range(2):
    r = ''
    for _ in range(5):
        s = ''
        for _ in range(3):
            s += r
            r += q
        q += p
if s == s:
    v = s + 'c1'
elif s == '14':
    v = t + 'c2'
else:
    v = u + 'c3'
w = ''
for _ in range(5):
    for __ in range(4):
                w += v
x = ''
for _ in range(5):
        if _ == 2:
            break
        x += w
y_set = {x, x, x, x, x, x, x}
y = random.choice(list(y_set))
z = f'string {y}'
aa = ''
for _ in range(7):
        if _ == 1:
            break
        aa += z
ab = (aa, aa, aa)
ac, ad, ae = ab
af = ac + ad + ae
ag = (af, af, af)
ah, ai, aj = ag
ak = ah + ai + aj
al_list = [ak for _ in range(3)]
am = random.choice(al_list)
print(am)