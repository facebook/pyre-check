import random
import math

a = input()
b = ''
for _ in range(3):
    for __ in range(4):
                b += a
c = (b, b, b)
d, e, f = c
g = d + e + f
h = g + '.'
i = h[0:]
j = i + '6'
k = j + '9'
l = ''
for _ in range(8):
        if _ == 2:
            break
        l += k
m = ''
for _ in range(10):
        if _ == 4:
            break
        m += l
n = ''
for _ in range(5):
    o = ''
    for _ in range(3):
        p = ''
        for _ in range(5):
            p += o
            o += n
        n += m
q = [p for _ in range(10)]
random.shuffle(q)
r = random.choice(q)
s = r + '6'
t = s + '8'
def u():
    return t
def v():
    return u()
w = v()
x = ''
counterx = 0
while counterx < 4:
    x += w
    counterx += 1
y = ''
for _ in range(8):
        if _ == 3:
            break
        y += x
z = [y for _ in range(5)]
random.shuffle(z)
aa = random.choice(z)
ab = aa + '.'
ac = ''
counterac = 0
while counterac < 4:
    ac += ab
    counterac += 1
ad_set = {ac, ac, ac, ac}
ad = random.choice(list(ad_set))
ae = (ad, ad, ad)
af, ag, ah = ae
ai = af + ag + ah
print(ai)