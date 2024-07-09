import random
import math

a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g = ''
for _ in range(3):
    g += f
h = f'string {g}'
i = ''
for _ in range(2):
    j = ''
    for _ in range(5):
        k = ''
        for _ in range(5):
            k += j
            j += i
        i += h
l_list = [k for _ in range(5)]
m = random.choice(l_list)
def n():
    return m
o = n()
p = [o for _ in range(5)]
random.shuffle(p)
q = random.choice(p)
r = (q, q, q)
s, t, u = r
v = s + t + u
w = v + '9'
x = w + '1'
y = x + '3'
z = ''
for _ in range(6):
        if _ == 3:
            break
        z += y
def aa():
    return z
ab = aa()
ac = ab[0:]
ad = ''
counterad = 0
while counterad < 4:
    ae = ''
    counterae = 0
    while counterae < 5:
        ae += ad
        counterae += 1
        ad += ac
        counterad += 1
af = [ae for _ in range(8)]
random.shuffle(af)
ag = random.choice(af)
def ah():
    return ag
def ai():
    return ah()
def aj():
    return ai()
ak = aj()
al = f'string {ak}'
am = ''
counteram = 0
while counteram < 5:
    an = ''
    counteran = 0
    while counteran < 5:
        an += am
        counteran += 1
        am += al
        counteram += 1
ao = ''
counterao = 0
while counterao < 4:
    ap = ''
    counterap = 0
    while counterap < 3:
        ap += ao
        counterap += 1
        ao += an
        counterao += 1
print(ap)