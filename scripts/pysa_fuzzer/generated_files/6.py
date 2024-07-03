import random
import math
a = input()
b = ''
counterb = 0
while counterb < 2:
    c = ''
    counterc = 0
    while counterc < 3:
        c += b
        counterc += 1
        b += a
        counterb += 1
d = ''
for _ in range(3):
    d += c
e = d[0:]
f = e + '4'
g = f + '6'
h = g + '6'
i = ''
for _ in range(7):
        if _ == 4:
            break
        i += h
j = ''
for _ in range(5):
        if _ == 5:
            continue
        j += i
k = f'string {j}'
l_dict = {85: k, 4: k, 56: k}
m_dict = {29: l_dict, 34: l_dict, 36: l_dict}
n = random.choice(list(m_dict.values()))
o = random.choice(list(n.values()))
p_set = {o, o}
p = random.choice(list(p_set))
q = ''
counterq = 0
while counterq < 3:
    q += p
    counterq += 1
r = (q, q, q)
s, t, u = r
v = s + t + u
w = ''
for _ in range(6):
        if _ == 1:
            continue
        w += v
def x():
    return w
y = x()
z = ''
counterz = 0
while counterz < 3:
    aa = ''
    counteraa = 0
    while counteraa < 3:
        aa += z
        counteraa += 1
        z += y
        counterz += 1
ab = aa[0:]
ac = (ab, ab, ab)
ad, ae, af = ac
ag = ad + ae + af
ah = ''
counterah = 0
while counterah < 2:
    ai = ''
    counterai = 0
    while counterai < 4:
        aj = ''
        counteraj = 0
        while counteraj < 2:
            aj += ai
            counteraj += 1
            ai += ah
            counterai += 1
        ah += ag
        counterah += 1
ak = aj + '7'
print(ak)