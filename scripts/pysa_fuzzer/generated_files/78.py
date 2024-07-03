import random
import math
a = input()
b_list = [a for _ in range(9)]
c_list = [b_list for _ in range(7)]
d = random.choice(c_list)
e = random.choice(d)
f = (e, e, e)
g, h, i = f
j = g + h + i
k = (j, j, j)
l, m, n = k
o = l + m + n
if o == '10':
    p = o + ' c1'
elif o == '14':
    p = o + ' c2'
else:
    p = o + ' c3'
def q():
    return p
def r():
    return q()
s = r()
t = ''
for _ in range(6):
        if _ == 2:
            continue
        t += s
def u():
    return t
def v():
    return u()
def w():
    return v()
x = w()
y_set = {x, x, x, x, x, x, x}
y = random.choice(list(y_set))
z = (y, y, y)
aa, ab, ac = z
ad = aa + ab + ac
if ad == '2':
    ae = ad + ' c1'
elif ad == '18':
    ae = ad + ' c2'
else:
    ae = ad + ' c3'
af = ''
for _ in range(7):
        if _ == 1:
            break
        af += ae
ag = af[0:]
ah = ''
for _ in range(3):
    ai = ''
    for _ in range(2):
        aj = ''
        for _ in range(2):
            aj += ai
            ai += ah
        ah += ag
ak = ''
counterak = 0
while counterak < 5:
    al = ''
    counteral = 0
    while counteral < 4:
        al += ak
        counteral += 1
        ak += aj
        counterak += 1
am = (al, al, al)
an, ao, ap = am
aq = an + ao + ap
ar = ''
for _ in range(4):
    for __ in range(2):
                ar += aq
at = ''
for _ in range(2):
    at += ar
au = ''
counterau = 0
while counterau < 3:
    au += at
    counterau += 1
print(au)