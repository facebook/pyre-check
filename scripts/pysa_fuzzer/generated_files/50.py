import random
import math
a = input()
b = [a for _ in range(6)]
random.shuffle(b)
c = random.choice(b)
d_list = [c for _ in range(5)]
e = random.choice(d_list)
f = ''
for _ in range(8):
        if _ == 2:
            continue
        f += e
g_list = [f for _ in range(5)]
h_list = [g_list for _ in range(6)]
i = random.choice(h_list)
j = random.choice(i)
k = ''
for _ in range(4):
    for __ in range(4):
                k += j
l = [k for _ in range(7)]
random.shuffle(l)
m = random.choice(l)
n = m[0:]
o = (n, n, n)
p, q, r = o
s = p + q + r
t = (s, s, s)
u, v, w = t
x = u + v + w
y = f'string {x}'
z = ''
for _ in range(3):
    for __ in range(4):
                z += y
aa = f'string {z}'
ab = f'string {aa}'
ac = ''
for _ in range(3):
    ad = ''
    for _ in range(2):
        ae = ''
        for _ in range(4):
            ae += ad
            ad += ac
        ac += ab
af = ''
counteraf = 0
while counteraf < 5:
    ag = ''
    counterag = 0
    while counterag < 4:
        ag += af
        counterag += 1
        af += ae
        counteraf += 1
ah_set = {ag, ag, ag}
ah = random.choice(list(ah_set))
ai = ah[0:]
aj_list = [ai for _ in range(4)]
ak_list = [aj_list for _ in range(4)]
al_list = [ak_list for _ in range(9)]
am = random.choice(al_list)
an = random.choice(am)
ao = random.choice(an)
print(ao)