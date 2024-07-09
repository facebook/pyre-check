import random
import math

a = input()
b_set = {a, a, a}
b = random.choice(list(b_set))
c = ''
for _ in range(3):
    for __ in range(5):
                c += b
d_list = [c for _ in range(6)]
e_list = [d_list for _ in range(3)]
f = random.choice(e_list)
g = random.choice(f)
h = g + '1'
i = h + '1'
j = i + '1'
def k():
    return j
def l():
    return k()
def m():
    return l()
n = m()
o = (n, n, n)
p, q, r = o
s = p + q + r
t = ''
for _ in range(2):
    u = ''
    for _ in range(5):
        u += t
        t += s
v = ''
for _ in range(5):
        if _ == 5:
            break
        v += u
w = ''
counterw = 0
while counterw < 4:
    x = ''
    counterx = 0
    while counterx < 5:
        y = ''
        countery = 0
        while countery < 3:
            y += x
            countery += 1
            x += w
            counterx += 1
        w += v
        counterw += 1
z = y + '2'
aa = z + '5'
ab = aa + '8'
def ac():
    return ab
ad = ac()
ae = ad + '.'
af = ''
for _ in range(3):
    ag = ''
    for _ in range(4):
        ag += af
        af += ae
ah_set = {ag, ag, ag}
ah = random.choice(list(ah_set))
ai = f'string {ah}'
aj = ''
for _ in range(3):
    for __ in range(5):
                aj += ai
ak = ''
counterak = 0
while counterak < 3:
    al = ''
    counteral = 0
    while counteral < 4:
        am = ''
        counteram = 0
        while counteram < 3:
            am += al
            counteram += 1
            al += ak
            counteral += 1
        ak += aj
        counterak += 1
an_list = [am for _ in range(4)]
ao_list = [an_list for _ in range(10)]
ap_list = [ao_list for _ in range(4)]
aq = random.choice(ap_list)
ar = random.choice(aq)
at = random.choice(ar)
print(at)