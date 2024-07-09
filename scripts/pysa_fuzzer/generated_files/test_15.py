import random
import math

a = input()
b = ''
for _ in range(2):
    b += a
c = ''
counterc = 0
while counterc < 2:
    d = ''
    counterd = 0
    while counterd < 2:
        d += c
        counterd += 1
        c += b
        counterc += 1
e = [d for _ in range(9)]
random.shuffle(e)
f = random.choice(e)
g = ''
counterg = 0
while counterg < 2:
    h = ''
    counterh = 0
    while counterh < 3:
        h += g
        counterh += 1
        g += f
        counterg += 1
i = h + '5'
j = i + '3'
k = j + '1'
l = k + '1'
m = ''
for _ in range(4):
    for __ in range(3):
                m += l
n_set = {m, m, m, m, m, m, m, m}
n = random.choice(list(n_set))
o = (n, n, n)
p, q, r = o
s = p + q + r
t = f'string {s}'
u_list = [t for _ in range(10)]
v_list = [u_list for _ in range(9)]
w_list = [v_list for _ in range(9)]
x = random.choice(w_list)
y = random.choice(x)
z = random.choice(y)
if z == z:
    ac = z + 'c1'
elif z == '14':
    ac = aa + 'c2'
else:
    ac = ab + 'c3'
ad = ''
for _ in range(3):
    for __ in range(3):
                ad += ac
if ad == ad:
    ag = ad + 'c1'
elif ad == '18':
    ag = ae + 'c2'
else:
    ag = af + 'c3'
ah_list = [ag for _ in range(2)]
ai = random.choice(ah_list)
if ai == ai:
    al = ai + 'c1'
elif ai == '17':
    al = aj + 'c2'
else:
    al = ak + 'c3'
am = ''
for _ in range(2):
    an = ''
    for _ in range(2):
        an += am
        am += al
ao = (an, an, an)
ap, aq, ar = ao
at = ap + aq + ar
print(at)