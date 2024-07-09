import random
import math

a = input()
b = a + '3'
c = b + '9'
def d():
    return c
def e():
    return d()
f = e()
g = (f, f, f)
h, i, j = g
k = h + i + j
l = f'string {k}'
if l == l:
    o = l + 'c1'
elif l == '12':
    o = m + 'c2'
else:
    o = n + 'c3'
p_set = {o, o, o, o, o, o, o, o, o, o}
p = random.choice(list(p_set))
q = (p, p, p)
r, s, t = q
u = r + s + t
v = f'string {u}'
w_list = [v for _ in range(5)]
x = random.choice(w_list)
y = ''
for _ in range(8):
        if _ == 2:
            continue
        y += x
z_list = [y for _ in range(5)]
aa = random.choice(z_list)
ab = ''
counterab = 0
while counterab < 2:
    ac = ''
    counterac = 0
    while counterac < 2:
        ac += ab
        counterac += 1
        ab += aa
        counterab += 1
ad = [ac for _ in range(10)]
random.shuffle(ad)
ae = random.choice(ad)
af = ''
for _ in range(6):
        if _ == 2:
            continue
        af += ae
ag = af[0:]
ah_list = [ag for _ in range(3)]
ai = random.choice(ah_list)
aj = ''
for _ in range(4):
    ak = ''
    for _ in range(3):
        al = ''
        for _ in range(5):
            al += ak
            ak += aj
        aj += ai
am = [al for _ in range(7)]
random.shuffle(am)
an = random.choice(am)
print(an)