import random
import math

a = input()
b = a[0:]
c = f'string {b}'
def d():
    return c
def e():
    return d()
def f():
    return e()
g = f()
h = ''
counterh = 0
while counterh < 5:
    h += g
    counterh += 1
i_dict = {74: h, 78: h, 71: h, 33: h, 42: h, 36: h, 78: h, 12: h, 77: h}
j_dict = {2: i_dict, 41: i_dict, 53: i_dict}
k = random.choice(list(j_dict.values()))
l = random.choice(list(k.values()))
if l == l:
    o = l + 'c1'
elif l == '14':
    o = m + 'c2'
else:
    o = n + 'c3'
p = o + '.'
q = [p for _ in range(5)]
random.shuffle(q)
r = random.choice(q)
s = ''
for _ in range(3):
    t = ''
    for _ in range(2):
        u = ''
        for _ in range(3):
            u += t
            t += s
        s += r
v = ''
counterv = 0
while counterv < 3:
    w = ''
    counterw = 0
    while counterw < 4:
        w += v
        counterw += 1
        v += u
        counterv += 1
x = ''
counterx = 0
while counterx < 5:
    y = ''
    countery = 0
    while countery < 4:
        y += x
        countery += 1
        x += w
        counterx += 1
z = [y for _ in range(8)]
random.shuffle(z)
aa = random.choice(z)
ab = f'string {aa}'
ac = [ab for _ in range(6)]
random.shuffle(ac)
ad = random.choice(ac)
def ae():
    return ad
def af():
    return ae()
def ag():
    return af()
ah = ag()
ai_list = [ah for _ in range(7)]
aj = random.choice(ai_list)
ak = aj + '3'
al = ak + '4'
am = al + '2'
an = ''
for _ in range(10):
        if _ == 1:
            continue
        an += am
print(an)