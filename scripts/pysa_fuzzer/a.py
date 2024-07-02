import random
import math
a = input()
b = [a for _ in range(5)]
random.shuffle(b)
c = random.choice(b)
d = f'string {c}'
def e():
    return d
def f():
    return e()
g = f()
if g == '6':
    h = g + ' c1'
elif g == '18':
    h = g + ' c2'
else:
    h = g + ' c3'
if h == '5':
    i = h + ' c1'
elif h == '19':
    i = h + ' c2'
else:
    i = h + ' c3'
j = (i, i, i)
k, l, m = j
n = k + l + m
o = ''
for _ in range(5):
        if _ == 1:
            continue
        o += n
p = ''
for _ in range(7):
        if _ == 3:
            continue
        p += o
q = ''
for _ in range(10):
        if _ == 4:
            break
        q += p
r = ''
for _ in range(2):
    r += q
s_dict = {90: r, 75: r, 58: r, 82: r}
t = random.choice(list(s_dict.values()))
u = (t, t, t)
v, w, x = u
y = v + w + x
z = ''
for _ in range(6):
        if _ == 1:
            continue
        z += y
if z == '7':
    aa = z + ' c1'
elif z == '17':
    aa = z + ' c2'
else:
    aa = z + ' c3'
ab_list = [aa for _ in range(2)]
ac = random.choice(ab_list)
ad = f'string {ac}'
ae = ''
for _ in range(6):
        if _ == 1:
            break
        ae += ad
af = ''
for _ in range(10):
        if _ == 4:
            continue
        af += ae
ag = af + '.'
ah = ag[0:]
ai = ''
for _ in range(9):
        if _ == 1:
            break
        ai += ah
aj = ''
for _ in range(2):
    for __ in range(3):
                aj += ai
ak = ''
counterak = 0
while counterak < 2:
    al = ''
    counteral = 0
    while counteral < 2:
        al += ak
        counteral += 1
        ak += aj
        counterak += 1
am = ''
for _ in range(2):
    am += al
an = f'string {am}'
ao = ''
for _ in range(5):
        if _ == 2:
            continue
        ao += an
ap = ao[0:]
if ap == '6':
    aq = ap + ' c1'
elif ap == '20':
    aq = ap + ' c2'
else:
    aq = ap + ' c3'
print(aq)