import random
import math

a = input()
b = ''
for _ in range(5):
    c = ''
    for _ in range(5):
        c += b
        b += a
d = ''
for _ in range(5):
    for __ in range(3):
                d += c
e = [d for _ in range(10)]
random.shuffle(e)
f = random.choice(e)
g = ''
counterg = 0
while counterg < 3:
    g += f
    counterg += 1
h = [g for _ in range(7)]
random.shuffle(h)
i = random.choice(h)
j = ''
for _ in range(5):
    for __ in range(5):
                j += i
k = j + '.'
l = ''
for _ in range(5):
        if _ == 5:
            break
        l += k
m = (l, l, l)
n, o, p = m
q = n + o + p
r_set = {q, q, q, q}
r = random.choice(list(r_set))
if r == r:
    u = r + 'c1'
elif r == '19':
    u = s + 'c2'
else:
    u = t + 'c3'
v = (u, u, u)
w, x, y = v
z = w + x + y
aa = ''
for _ in range(6):
        if _ == 4:
            continue
        aa += z
def ab():
    return aa
def ac():
    return ab()
def ad():
    return ac()
ae = ad()
af = [ae for _ in range(6)]
random.shuffle(af)
ag = random.choice(af)
ah = ag + '5'
ai = ah + '1'
aj = (ai, ai, ai)
ak, al, am = aj
an = ak + al + am
ao_list = [an for _ in range(10)]
ap_list = [ao_list for _ in range(5)]
aq = random.choice(ap_list)
ar = random.choice(aq)
print(ar)