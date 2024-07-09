import random
import math

a = input()
b = a[0:]
c = [b for _ in range(5)]
random.shuffle(c)
d = random.choice(c)
e = d + '.'
f = ''
for _ in range(4):
    g = ''
    for _ in range(3):
        h = ''
        for _ in range(5):
            h += g
            g += f
        f += e
i = ''
counteri = 0
while counteri < 4:
    j = ''
    counterj = 0
    while counterj < 4:
        j += i
        counterj += 1
        i += h
        counteri += 1
k_set = {j, j, j, j}
k = random.choice(list(k_set))
l_list = [k for _ in range(4)]
m_list = [l_list for _ in range(5)]
n_list = [m_list for _ in range(3)]
o = random.choice(n_list)
p = random.choice(o)
q = random.choice(p)
r = (q, q, q)
s, t, u = r
v = s + t + u
w = [v for _ in range(6)]
random.shuffle(w)
x = random.choice(w)
y = ''
for _ in range(10):
        if _ == 4:
            break
        y += x
z = y + '1'
aa = z + '4'
ab = ''
counterab = 0
while counterab < 3:
    ac = ''
    counterac = 0
    while counterac < 3:
        ad = ''
        counterad = 0
        while counterad < 2:
            ad += ac
            counterad += 1
            ac += ab
            counterac += 1
        ab += aa
        counterab += 1
ae = [ad for _ in range(7)]
random.shuffle(ae)
af = random.choice(ae)
ag = ''
for _ in range(5):
    ah = ''
    for _ in range(2):
        ai = ''
        for _ in range(2):
            ai += ah
            ah += ag
        ag += af
aj = ai + '6'
ak = aj + '1'
al = ak[0:]
am = (al, al, al)
an, ao, ap = am
aq = an + ao + ap
ar = ''
for _ in range(4):
    for __ in range(2):
                ar += aq
print(ar)