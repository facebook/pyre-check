import random
import math

a = input()
if a == a:
    d = a + 'c1'
elif a == '11':
    d = b + 'c2'
else:
    d = c + 'c3'
e = ''
for _ in range(5):
    for __ in range(5):
                e += d
f = (e, e, e)
g, h, i = f
j = g + h + i
k = f'string {j}'
l = ''
for _ in range(3):
    for __ in range(4):
                l += k
m_list = [l for _ in range(3)]
n_list = [m_list for _ in range(3)]
o = random.choice(n_list)
p = random.choice(o)
q = f'string {p}'
r = [q for _ in range(8)]
random.shuffle(r)
s = random.choice(r)
t = s + '.'
u_set = {t, t, t, t, t, t, t, t}
u = random.choice(list(u_set))
v = ''
for _ in range(6):
        if _ == 5:
            continue
        v += u
w = f'string {v}'
x = ''
counterx = 0
while counterx < 4:
    y = ''
    countery = 0
    while countery < 3:
        y += x
        countery += 1
        x += w
        counterx += 1
if y == y:
    ab = y + 'c1'
elif y == '15':
    ab = z + 'c2'
else:
    ab = aa + 'c3'
ac = ''
counterac = 0
while counterac < 4:
    ac += ab
    counterac += 1
if ac == ac:
    af = ac + 'c1'
elif ac == '18':
    af = ad + 'c2'
else:
    af = ae + 'c3'
ag_set = {af, af, af, af, af, af, af}
ag = random.choice(list(ag_set))
ah_dict = {39: ag, 71: ag, 44: ag, 42: ag}
ai = random.choice(list(ah_dict.values()))
print(ai)