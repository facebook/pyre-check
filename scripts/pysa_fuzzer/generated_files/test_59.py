import random
import math

a = input()
b = a + '.'
c = b[0:]
d = ''
for _ in range(4):
    e = ''
    for _ in range(3):
        f = ''
        for _ in range(2):
            f += e
            e += d
        d += c
g = ''
counterg = 0
while counterg < 2:
    h = ''
    counterh = 0
    while counterh < 2:
        h += g
        counterh += 1
        g += f
        counterg += 1
if h == h:
    k = h + 'c1'
elif h == '13':
    k = i + 'c2'
else:
    k = j + 'c3'
l = ''
for _ in range(3):
    m = ''
    for _ in range(4):
        n = ''
        for _ in range(5):
            n += m
            m += l
        l += k
o = n + '.'
p = ''
for _ in range(3):
    for __ in range(2):
                p += o
q = p + '.'
if q == q:
    t = q + 'c1'
elif q == '16':
    t = r + 'c2'
else:
    t = s + 'c3'
u_dict = {46: t, 89: t, 29: t}
v_dict = {61: u_dict, 91: u_dict, 37: u_dict, 59: u_dict, 49: u_dict, 51: u_dict, 88: u_dict, 6: u_dict, 84: u_dict, 54: u_dict}
w = random.choice(list(v_dict.values()))
x = random.choice(list(w.values()))
y = x + '.'
z = ''
for _ in range(10):
        if _ == 3:
            break
        z += y
aa = z[0:]
ab = aa[0:]
ac = (ab, ab, ab)
ad, ae, af = ac
ag = ad + ae + af
ah = [ag for _ in range(10)]
random.shuffle(ah)
ai = random.choice(ah)
aj_set = {ai, ai}
aj = random.choice(list(aj_set))
print(aj)