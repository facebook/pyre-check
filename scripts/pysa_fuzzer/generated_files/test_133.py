import random
import math

a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g_set = {f, f, f}
g = random.choice(list(g_set))
h = ''
for _ in range(10):
        if _ == 5:
            continue
        h += g
i = h + '7'
j = i + '9'
k = j + '9'
def l():
    return k
def m():
    return l()
def n():
    return m()
o = n()
p = ''
counterp = 0
while counterp < 3:
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
s = (r, r, r)
t, u, v = s
w = t + u + v
def x():
    return w
y = x()
z = y + '.'
aa = ''
counteraa = 0
while counteraa < 2:
    aa += z
    counteraa += 1
ab_set = {aa, aa, aa}
ab = random.choice(list(ab_set))
ac = ab[0:]
ad = ''
for _ in range(5):
    ae = ''
    for _ in range(4):
        af = ''
        for _ in range(3):
            af += ae
            ae += ad
        ad += ac
ag = f'string {af}'
ah = f'string {ag}'
def ai():
    return ah
def aj():
    return ai()
def ak():
    return aj()
al = ak()
am = ''
for _ in range(9):
        if _ == 5:
            break
        am += al
an = ''
for _ in range(5):
    for __ in range(4):
                an += am
print(an)