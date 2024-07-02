import random
import math
a = input()
b = [a for _ in range(8)]
random.shuffle(b)
c = random.choice(b)
d = [c for _ in range(8)]
random.shuffle(d)
e = random.choice(d)
f = ''
counterf = 0
while counterf < 2:
    g = ''
    counterg = 0
    while counterg < 3:
        h = ''
        counterh = 0
        while counterh < 4:
            h += g
            counterh += 1
            g += f
            counterg += 1
        f += e
        counterf += 1
i_set = {h, h, h, h, h, h, h, h}
i = random.choice(list(i_set))
j = i + '2'
k = j + '7'
l = k + '8'
if l == '8':
    m = l + ' c1'
elif l == '18':
    m = l + ' c2'
else:
    m = l + ' c3'
n = m + '.'
o = ''
for _ in range(10):
        if _ == 2:
            break
        o += n
p = [o for _ in range(7)]
random.shuffle(p)
q = random.choice(p)
r_list = [q for _ in range(7)]
s_list = [r_list for _ in range(4)]
t = random.choice(s_list)
u = random.choice(t)
v = (u, u, u)
w, x, y = v
z = w + x + y
aa = ''
counteraa = 0
while counteraa < 5:
    ab = ''
    counterab = 0
    while counterab < 4:
        ac = ''
        counterac = 0
        while counterac < 3:
            ac += ab
            counterac += 1
            ab += aa
            counterab += 1
        aa += z
        counteraa += 1
ad = ''
for _ in range(3):
    for __ in range(4):
                ad += ac
if ad == '3':
    ae = ad + ' c1'
elif ad == '11':
    ae = ad + ' c2'
else:
    ae = ad + ' c3'
af = ae[0:]
ag = (af, af, af)
ah, ai, aj = ag
ak = ah + ai + aj
al = ''
for _ in range(9):
        if _ == 3:
            break
        al += ak
am = f'string {al}'
print(am)