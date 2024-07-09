import random
import math

a = input()
def b():
    return a
def c():
    return b()
def d():
    return c()
e = d()
f = ''
for _ in range(3):
    f += e
g = ''
counterg = 0
while counterg < 2:
    h = ''
    counterh = 0
    while counterh < 2:
        i = ''
        counteri = 0
        while counteri < 4:
            i += h
            counteri += 1
            h += g
            counterh += 1
        g += f
        counterg += 1
j = f'string {i}'
k = [j for _ in range(7)]
random.shuffle(k)
l = random.choice(k)
m = [l for _ in range(8)]
random.shuffle(m)
n = random.choice(m)
o_dict = {83: n, 98: n, 11: n, 92: n, 5: n, 18: n, 32: n, 11: n, 92: n}
p_dict = {73: o_dict, 39: o_dict, 45: o_dict, 51: o_dict}
q_dict = {3: p_dict, 92: p_dict}
r = random.choice(list(q_dict.values()))
s = random.choice(list(r.values()))
t = random.choice(list(s.values()))
u = t + '2'
v = u + '4'
w = v + '8'
x_dict = {84: w, 3: w, 52: w, 11: w, 39: w, 97: w, 56: w, 70: w, 8: w, 37: w}
y_dict = {88: x_dict, 34: x_dict}
z_dict = {99: y_dict, 68: y_dict, 23: y_dict, 31: y_dict, 62: y_dict, 63: y_dict}
aa = random.choice(list(z_dict.values()))
ab = random.choice(list(aa.values()))
ac = random.choice(list(ab.values()))
def ad():
    return ac
ae = ad()
af = ''
for _ in range(2):
    ag = ''
    for _ in range(3):
        ah = ''
        for _ in range(5):
            ah += ag
            ag += af
        af += ae
ai = [ah for _ in range(10)]
random.shuffle(ai)
aj = random.choice(ai)
ak = [aj for _ in range(8)]
random.shuffle(ak)
al = random.choice(ak)
if al == al:
    ao = al + 'c1'
elif al == '16':
    ao = am + 'c2'
else:
    ao = an + 'c3'
ap = f'string {ao}'
aq = ''
for _ in range(5):
    for __ in range(2):
                aq += ap
ar = [aq for _ in range(9)]
random.shuffle(ar)
at = random.choice(ar)
au = ''
counterau = 0
while counterau < 5:
    av = ''
    counterav = 0
    while counterav < 5:
        av += au
        counterav += 1
        au += at
        counterau += 1
print(av)