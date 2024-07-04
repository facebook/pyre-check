import random
import math
a = input()
b = ''
counterb = 0
while counterb < 5:
    c = ''
    counterc = 0
    while counterc < 3:
        c += b
        counterc += 1
        b += a
        counterb += 1
d_set = {c, c, c, c}
d = random.choice(list(d_set))
e_set = {d, d, d, d, d, d, d}
e = random.choice(list(e_set))
f = e[0:]
g = ''
for _ in range(4):
    h = ''
    for _ in range(2):
        i = ''
        for _ in range(3):
            i += h
            h += g
        g += f
j = ''
for _ in range(3):
    for __ in range(3):
                j += i
k = ''
for _ in range(4):
    l = ''
    for _ in range(5):
        l += k
        k += j
m = (l, l, l)
n, o, p = m
q = n + o + p
r = (q, q, q)
s, t, u = r
v = s + t + u
w = ''
counterw = 0
while counterw < 4:
    x = ''
    counterx = 0
    while counterx < 5:
        x += w
        counterx += 1
        w += v
        counterw += 1
y_list = [x for _ in range(9)]
z = random.choice(y_list)
if z == '6':
    aa = z + ' c1'
elif z == '17':
    aa = z + ' c2'
else:
    aa = z + ' c3'
ab_set = {aa, aa, aa, aa, aa, aa, aa, aa, aa}
ab = random.choice(list(ab_set))
def ac():
    return ab
ad = ac()
ae = ''
for _ in range(5):
        if _ == 3:
            continue
        ae += ad
af_list = [ae for _ in range(5)]
ag = random.choice(af_list)
ah = ag[0:]
ai = ''
for _ in range(5):
        if _ == 5:
            break
        ai += ah
print(ai)