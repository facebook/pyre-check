import random
import math

a = input()
b = ''
for _ in range(6):
        if _ == 4:
            break
        b += a
c_set = {b, b, b, b, b, b, b, b, b, b}
c = random.choice(list(c_set))
d_list = [c for _ in range(2)]
e_list = [d_list for _ in range(7)]
f_list = [e_list for _ in range(8)]
g = random.choice(f_list)
h = random.choice(g)
i = random.choice(h)
def j():
    return i
def k():
    return j()
l = k()
m = l + '.'
n = ''
countern = 0
while countern < 4:
    o = ''
    countero = 0
    while countero < 4:
        p = ''
        counterp = 0
        while counterp < 4:
            p += o
            counterp += 1
            o += n
            countero += 1
        n += m
        countern += 1
if p == p:
    s = p + 'c1'
elif p == '14':
    s = q + 'c2'
else:
    s = r + 'c3'
t = s + '9'
u = t + '3'
v = u + '6'
w = ''
for _ in range(2):
    x = ''
    for _ in range(5):
        y = ''
        for _ in range(3):
            y += x
            x += w
        w += v
z = ''
for _ in range(9):
        if _ == 4:
            continue
        z += y
aa = [z for _ in range(10)]
random.shuffle(aa)
ab = random.choice(aa)
ac = ''
counterac = 0
while counterac < 5:
    ac += ab
    counterac += 1
ad = (ac, ac, ac)
ae, af, ag = ad
ah = ae + af + ag
ai = ''
for _ in range(4):
    for __ in range(2):
                ai += ah
aj_list = [ai for _ in range(4)]
ak = random.choice(aj_list)
al_dict = {57: ak, 17: ak, 46: ak, 78: ak}
am_dict = {100: al_dict, 77: al_dict, 30: al_dict, 33: al_dict, 66: al_dict}
an_dict = {73: am_dict, 86: am_dict, 3: am_dict, 11: am_dict, 91: am_dict, 24: am_dict, 100: am_dict}
ao = random.choice(list(an_dict.values()))
ap = random.choice(list(ao.values()))
aq = random.choice(list(ap.values()))
ar = ''
for _ in range(8):
        if _ == 5:
            break
        ar += aq
at = ''
for _ in range(2):
    for __ in range(3):
                at += ar
print(at)