import random
import math
a = input()
b = f'string {a}'
c = ''
for _ in range(10):
        if _ == 2:
            break
        c += b
d = c + '.'
e = (d, d, d)
f, g, h = e
i = f + g + h
j = ''
for _ in range(3):
    k = ''
    for _ in range(3):
        l = ''
        for _ in range(5):
            l += k
            k += j
        j += i
m_dict = {14: l, 21: l, 48: l, 85: l, 92: l, 66: l, 85: l}
n_dict = {54: m_dict, 69: m_dict, 41: m_dict, 34: m_dict}
o = random.choice(list(n_dict.values()))
p = random.choice(list(o.values()))
q = ''
for _ in range(2):
    for __ in range(4):
                q += p
r = q + '9'
s = r + '4'
t = s + '8'
u = (t, t, t)
v, w, x = u
y = v + w + x
z = y + '6'
aa = z + '2'
ab = aa + '1'
ac = ''
for _ in range(2):
    ac += ab
ad = [ac for _ in range(6)]
random.shuffle(ad)
ae = random.choice(ad)
af = ''
counteraf = 0
while counteraf < 5:
    ag = ''
    counterag = 0
    while counterag < 4:
        ah = ''
        counterah = 0
        while counterah < 4:
            ah += ag
            counterah += 1
            ag += af
            counterag += 1
        af += ae
        counteraf += 1
ai = ''
for _ in range(9):
        if _ == 4:
            continue
        ai += ah
aj = ai + '4'
ak_list = [aj for _ in range(6)]
al = random.choice(ak_list)
am = ''
for _ in range(5):
    for __ in range(3):
                am += al
an = ''
for _ in range(3):
    ao = ''
    for _ in range(5):
        ap = ''
        for _ in range(2):
            ap += ao
            ao += an
        an += am
print(ap)