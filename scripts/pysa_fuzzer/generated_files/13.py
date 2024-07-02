import random
import math
a = input()
b_set = {a, a, a, a, a, a, a}
b = random.choice(list(b_set))
c = ''
counterc = 0
while counterc < 4:
    d = ''
    counterd = 0
    while counterd < 4:
        e = ''
        countere = 0
        while countere < 4:
            e += d
            countere += 1
            d += c
            counterd += 1
        c += b
        counterc += 1
f = (e, e, e)
g, h, i = f
j = g + h + i
k = j[0:]
if k == '4':
    l = k + ' c1'
elif k == '13':
    l = k + ' c2'
else:
    l = k + ' c3'
m = ''
for _ in range(5):
    n = ''
    for _ in range(3):
        o = ''
        for _ in range(2):
            o += n
            n += m
        m += l
p = [o for _ in range(5)]
random.shuffle(p)
q = random.choice(p)
r = q + '.'
s = ''
counters = 0
while counters < 3:
    t = ''
    countert = 0
    while countert < 2:
        t += s
        countert += 1
        s += r
        counters += 1
u_set = {t, t, t, t, t, t}
u = random.choice(list(u_set))
v_list = [u for _ in range(9)]
w_list = [v_list for _ in range(5)]
x = random.choice(w_list)
y = random.choice(x)
def z():
    return y
def aa():
    return z()
ab = aa()
ac = ab[0:]
ad_set = {ac, ac, ac, ac, ac, ac, ac, ac}
ad = random.choice(list(ad_set))
if ad == '6':
    ae = ad + ' c1'
elif ad == '14':
    ae = ad + ' c2'
else:
    ae = ad + ' c3'
af = f'string {ae}'
ag = af + '.'
ah = ag + '.'
print(ah)