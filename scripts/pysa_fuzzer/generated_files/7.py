import random
import math
a = input()
b = [a for _ in range(7)]
random.shuffle(b)
c = random.choice(b)
d = ''
counterd = 0
while counterd < 5:
    e = ''
    countere = 0
    while countere < 4:
        f = ''
        counterf = 0
        while counterf < 3:
            f += e
            counterf += 1
            e += d
            countere += 1
        d += c
        counterd += 1
def g():
    return f
def h():
    return g()
i = h()
j = (i, i, i)
k, l, m = j
n = k + l + m
o = f'string {n}'
p_list = [o for _ in range(4)]
q_list = [p_list for _ in range(5)]
r_list = [q_list for _ in range(4)]
s = random.choice(r_list)
t = random.choice(s)
u = random.choice(t)
v = [u for _ in range(7)]
random.shuffle(v)
w = random.choice(v)
def x():
    return w
def y():
    return x()
def z():
    return y()
aa = z()
ab = aa + '9'
if ab == '7':
    ac = ab + ' c1'
elif ab == '14':
    ac = ab + ' c2'
else:
    ac = ab + ' c3'
ad = ''
counterad = 0
while counterad < 3:
    ad += ac
    counterad += 1
ae = ''
counterae = 0
while counterae < 2:
    ae += ad
    counterae += 1
af_list = [ae for _ in range(5)]
ag_list = [af_list for _ in range(3)]
ah_list = [ag_list for _ in range(7)]
ai = random.choice(ah_list)
aj = random.choice(ai)
ak = random.choice(aj)
al = ''
for _ in range(2):
    am = ''
    for _ in range(3):
        am += al
        al += ak
an = f'string {am}'
ao = (an, an, an)
ap, aq, ar = ao
at = ap + aq + ar
au = f'string {at}'
av = ''
for _ in range(4):
    for __ in range(4):
                av += au
print(av)