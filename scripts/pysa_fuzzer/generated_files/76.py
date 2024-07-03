import random
import math
a = input()
b = a + '9'
c = b + '7'
d = c + '9'
e = ''
countere = 0
while countere < 2:
    f = ''
    counterf = 0
    while counterf < 2:
        g = ''
        counterg = 0
        while counterg < 3:
            g += f
            counterg += 1
            f += e
            counterf += 1
        e += d
        countere += 1
h = ''
for _ in range(8):
        if _ == 5:
            break
        h += g
def i():
    return h
def j():
    return i()
k = j()
l = k + '.'
m = l[0:]
n_set = {m, m, m, m, m, m, m, m}
n = random.choice(list(n_set))
o_list = [n for _ in range(7)]
p_list = [o_list for _ in range(6)]
q_list = [p_list for _ in range(5)]
r = random.choice(q_list)
s = random.choice(r)
t = random.choice(s)
def u():
    return t
def v():
    return u()
w = v()
x = w + '.'
y = [x for _ in range(7)]
random.shuffle(y)
z = random.choice(y)
aa = ''
for _ in range(9):
        if _ == 4:
            break
        aa += z
if aa == '3':
    ab = aa + ' c1'
elif aa == '14':
    ab = aa + ' c2'
else:
    ab = aa + ' c3'
ac = [ab for _ in range(9)]
random.shuffle(ac)
ad = random.choice(ac)
ae = (ad, ad, ad)
af, ag, ah = ae
ai = af + ag + ah
def aj():
    return ai
def ak():
    return aj()
al = ak()
am = (al, al, al)
an, ao, ap = am
aq = an + ao + ap
ar = ''
counterar = 0
while counterar < 5:
    ar += aq
    counterar += 1
print(ar)