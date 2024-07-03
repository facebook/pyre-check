import random
import math
a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g = f + '.'
h = g[0:]
def i():
    return h
def j():
    return i()
def k():
    return j()
l = k()
m = l + '.'
n = ''
for _ in range(2):
    for __ in range(2):
                n += m
o = n + '4'
p = o + '2'
q = p + '8'
r = q + '4'
s = r + '7'
t = s + '5'
u = (t, t, t)
v, w, x = u
y = v + w + x
z_list = [y for _ in range(3)]
aa_list = [z_list for _ in range(7)]
ab = random.choice(aa_list)
ac = random.choice(ab)
ad_set = {ac, ac, ac, ac, ac, ac, ac}
ad = random.choice(list(ad_set))
ae_set = {ad, ad}
ae = random.choice(list(ae_set))
af = ae + '4'
ag = af + '1'
ah = ag + '4'
ai = ''
counterai = 0
while counterai < 3:
    ai += ah
    counterai += 1
aj = ''
for _ in range(3):
    ak = ''
    for _ in range(2):
        ak += aj
        aj += ai
al_list = [ak for _ in range(3)]
am_list = [al_list for _ in range(3)]
an_list = [am_list for _ in range(7)]
ao = random.choice(an_list)
ap = random.choice(ao)
aq = random.choice(ap)
ar = ''
counterar = 0
while counterar < 5:
    at = ''
    counterat = 0
    while counterat < 3:
        au = ''
        counterau = 0
        while counterau < 5:
            au += at
            counterau += 1
            at += ar
            counterat += 1
        ar += aq
        counterar += 1
av = ''
counterav = 0
while counterav < 5:
    aw = ''
    counteraw = 0
    while counteraw < 5:
        aw += av
        counteraw += 1
        av += au
        counterav += 1
print(aw)