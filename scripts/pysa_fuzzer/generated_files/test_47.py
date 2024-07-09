import random
import math

a = input()
b = a + '.'
c = ''
for _ in range(5):
        if _ == 4:
            break
        c += b
d_dict = {39: c, 77: c, 13: c, 63: c, 72: c, 68: c, 91: c, 7: c}
e_dict = {44: d_dict, 25: d_dict, 53: d_dict, 66: d_dict, 83: d_dict, 40: d_dict, 33: d_dict}
f = random.choice(list(e_dict.values()))
g = random.choice(list(f.values()))
def h():
    return g
def i():
    return h()
def j():
    return i()
k = j()
l = ''
for _ in range(5):
    l += k
m = f'string {l}'
n = ''
for _ in range(4):
    o = ''
    for _ in range(2):
        o += n
        n += m
p = ''
counterp = 0
while counterp < 2:
    q = ''
    counterq = 0
    while counterq < 3:
        r = ''
        counterr = 0
        while counterr < 3:
            r += q
            counterr += 1
            q += p
            counterq += 1
        p += o
        counterp += 1
s = ''
counters = 0
while counters < 3:
    t = ''
    countert = 0
    while countert < 3:
        u = ''
        counteru = 0
        while counteru < 2:
            u += t
            counteru += 1
            t += s
            countert += 1
        s += r
        counters += 1
if u == u:
    x = u + 'c1'
elif u == '12':
    x = v + 'c2'
else:
    x = w + 'c3'
y = [x for _ in range(8)]
random.shuffle(y)
z = random.choice(y)
aa = ''
for _ in range(6):
        if _ == 4:
            continue
        aa += z
ab = aa + '.'
ac = ''
counterac = 0
while counterac < 2:
    ad = ''
    counterad = 0
    while counterad < 4:
        ae = ''
        counterae = 0
        while counterae < 5:
            ae += ad
            counterae += 1
            ad += ac
            counterad += 1
        ac += ab
        counterac += 1
af = (ae, ae, ae)
ag, ah, ai = af
aj = ag + ah + ai
ak = [aj for _ in range(7)]
random.shuffle(ak)
al = random.choice(ak)
def am():
    return al
def an():
    return am()
def ao():
    return an()
ap = ao()
if ap == ap:
    at = ap + 'c1'
elif ap == '11':
    at = aq + 'c2'
else:
    at = ar + 'c3'
print(at)