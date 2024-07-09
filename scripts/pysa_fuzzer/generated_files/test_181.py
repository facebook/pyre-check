import random
import math

a = input()
b = ''
for _ in range(5):
        if _ == 5:
            break
        b += a
c = b + '2'
d = c + '7'
e = d + '2'
f = (e, e, e)
g, h, i = f
j = g + h + i
k = j + '.'
l = [k for _ in range(10)]
random.shuffle(l)
m = random.choice(l)
def n():
    return m
def o():
    return n()
p = o()
q_set = {p, p, p, p, p}
q = random.choice(list(q_set))
r = ''
for _ in range(8):
        if _ == 2:
            continue
        r += q
s = ''
counters = 0
while counters < 4:
    t = ''
    countert = 0
    while countert < 2:
        t += s
        countert += 1
        s += r
        counters += 1
def u():
    return t
def v():
    return u()
def w():
    return v()
x = w()
y = ''
for _ in range(7):
        if _ == 5:
            break
        y += x
z = ''
counterz = 0
while counterz < 4:
    aa = ''
    counteraa = 0
    while counteraa < 3:
        ab = ''
        counterab = 0
        while counterab < 2:
            ab += aa
            counterab += 1
            aa += z
            counteraa += 1
        z += y
        counterz += 1
ac = ''
counterac = 0
while counterac < 4:
    ac += ab
    counterac += 1
ad = ''
for _ in range(2):
    ae = ''
    for _ in range(3):
        ae += ad
        ad += ac
af_list = [ae for _ in range(5)]
ag_list = [af_list for _ in range(2)]
ah = random.choice(ag_list)
ai = random.choice(ah)
aj_set = {ai, ai, ai, ai, ai, ai, ai}
aj = random.choice(list(aj_set))
ak_set = {aj, aj, aj, aj, aj, aj}
ak = random.choice(list(ak_set))
al = ''
for _ in range(3):
    am = ''
    for _ in range(5):
        am += al
        al += ak
print(am)