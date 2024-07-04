import random
import math
a = input()
b = ''
counterb = 0
while counterb < 3:
    c = ''
    counterc = 0
    while counterc < 3:
        c += b
        counterc += 1
        b += a
        counterb += 1
d = ''
for _ in range(4):
    for __ in range(3):
                d += c
def e():
    return d
f = e()
g_dict = {91: f, 60: f, 76: f, 53: f, 45: f, 57: f, 83: f, 44: f}
h_dict = {39: g_dict, 75: g_dict, 71: g_dict, 15: g_dict, 74: g_dict, 31: g_dict, 3: g_dict}
i = random.choice(list(h_dict.values()))
j = random.choice(list(i.values()))
k = f'string {j}'
l = f'string {k}'
m = [l for _ in range(5)]
random.shuffle(m)
n = random.choice(m)
o = (n, n, n)
p, q, r = o
s = p + q + r
t = ''
countert = 0
while countert < 2:
    u = ''
    counteru = 0
    while counteru < 5:
        v = ''
        counterv = 0
        while counterv < 5:
            v += u
            counterv += 1
            u += t
            counteru += 1
        t += s
        countert += 1
w = ''
for _ in range(3):
    w += v
def x():
    return w
def y():
    return x()
z = y()
def aa():
    return z
def ab():
    return aa()
def ac():
    return ab()
ad = ac()
ae = ad + '1'
af = ae + '1'
ag_list = [af for _ in range(4)]
ah_list = [ag_list for _ in range(8)]
ai = random.choice(ah_list)
aj = random.choice(ai)
if aj == '10':
    ak = aj + ' c1'
elif aj == '16':
    ak = aj + ' c2'
else:
    ak = aj + ' c3'
if ak == '1':
    al = ak + ' c1'
elif ak == '20':
    al = ak + ' c2'
else:
    al = ak + ' c3'
def am():
    return al
def an():
    return am()
ao = an()
ap = ''
counterap = 0
while counterap < 4:
    aq = ''
    counteraq = 0
    while counteraq < 2:
        ar = ''
        counterar = 0
        while counterar < 4:
            ar += aq
            counterar += 1
            aq += ap
            counteraq += 1
        ap += ao
        counterap += 1
print(ar)