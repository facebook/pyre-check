import random
import math
a = input()
b = a + '.'
c = ''
for _ in range(10):
        if _ == 2:
            break
        c += b
d = ''
for _ in range(5):
    for __ in range(5):
                d += c
e = f'string {d}'
f = e + '8'
g = ''
for _ in range(3):
    for __ in range(3):
                g += f
if g == '10':
    h = g + ' c1'
elif g == '15':
    h = g + ' c2'
else:
    h = g + ' c3'
i = [h for _ in range(7)]
random.shuffle(i)
j = random.choice(i)
k = ''
for _ in range(5):
    for __ in range(3):
                k += j
l_dict = {85: k, 47: k, 18: k, 85: k, 55: k, 18: k, 83: k, 61: k}
m = random.choice(list(l_dict.values()))
n = ''
for _ in range(5):
    o = ''
    for _ in range(5):
        o += n
        n += m
if o == '7':
    p = o + ' c1'
elif o == '11':
    p = o + ' c2'
else:
    p = o + ' c3'
q = (p, p, p)
r, s, t = q
u = r + s + t
v = ''
counterv = 0
while counterv < 4:
    w = ''
    counterw = 0
    while counterw < 3:
        w += v
        counterw += 1
        v += u
        counterv += 1
x = w[0:]
def y():
    return x
def z():
    return y()
def aa():
    return z()
ab = aa()
ac = (ab, ab, ab)
ad, ae, af = ac
ag = ad + ae + af
ah_set = {ag, ag, ag, ag, ag, ag, ag}
ah = random.choice(list(ah_set))
print(ah)