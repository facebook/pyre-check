import random
import math
a = input()
b = ''
for _ in range(5):
    for __ in range(3):
                b += a
c = ''
for _ in range(2):
    for __ in range(2):
                c += b
d = ''
counterd = 0
while counterd < 4:
    e = ''
    countere = 0
    while countere < 3:
        e += d
        countere += 1
        d += c
        counterd += 1
if e == '5':
    f = e + ' c1'
elif e == '14':
    f = e + ' c2'
else:
    f = e + ' c3'
g = f + '.'
h = g + '4'
i = h + '3'
j = i + '9'
k = j + '2'
l_dict = {73: k, 19: k, 90: k, 18: k, 88: k, 54: k, 38: k}
m_dict = {25: l_dict, 23: l_dict, 99: l_dict, 30: l_dict, 92: l_dict, 39: l_dict}
n_dict = {73: m_dict, 17: m_dict, 97: m_dict, 9: m_dict, 22: m_dict, 86: m_dict}
o = random.choice(list(n_dict.values()))
p = random.choice(list(o.values()))
q = random.choice(list(p.values()))
r = ''
counterr = 0
while counterr < 3:
    r += q
    counterr += 1
s = [r for _ in range(8)]
random.shuffle(s)
t = random.choice(s)
u_set = {t, t, t, t, t, t, t}
u = random.choice(list(u_set))
v = u[0:]
w_set = {v, v, v, v, v}
w = random.choice(list(w_set))
x_list = [w for _ in range(9)]
y_list = [x_list for _ in range(6)]
z = random.choice(y_list)
aa = random.choice(z)
ab = ''
for _ in range(5):
        if _ == 5:
            break
        ab += aa
ac = f'string {ab}'
if ac == '9':
    ad = ac + ' c1'
elif ac == '12':
    ad = ac + ' c2'
else:
    ad = ac + ' c3'
ae = ''
counterae = 0
while counterae < 4:
    ae += ad
    counterae += 1
af_set = {ae, ae, ae, ae, ae, ae, ae}
af = random.choice(list(af_set))
ag = [af for _ in range(9)]
random.shuffle(ag)
ah = random.choice(ag)
ai = ''
for _ in range(7):
        if _ == 2:
            break
        ai += ah
aj = ''
for _ in range(9):
        if _ == 1:
            continue
        aj += ai
ak = aj[0:]
al = [ak for _ in range(7)]
random.shuffle(al)
am = random.choice(al)
an = am + '1'
ao = an + '6'
ap = ao + '1'
aq = (ap, ap, ap)
ar, at, au = aq
av = ar + at + au
aw = ''
for _ in range(4):
    for __ in range(5):
                aw += av
ax = aw + '7'
ay = ax + '2'
az = ay + '5'
ba_set = {az, az, az, az, az, az, az, az}
ba = random.choice(list(ba_set))
bb_dict = {17: ba, 29: ba, 18: ba, 7: ba, 32: ba, 35: ba, 51: ba, 73: ba}
bc_dict = {37: bb_dict, 11: bb_dict, 90: bb_dict, 45: bb_dict, 22: bb_dict, 93: bb_dict, 35: bb_dict, 44: bb_dict, 96: bb_dict}
bd = random.choice(list(bc_dict.values()))
be = random.choice(list(bd.values()))
bf = be + '9'
bg = ''
for _ in range(4):
    for __ in range(4):
                bg += bf
bh_list = [bg for _ in range(10)]
bi_list = [bh_list for _ in range(10)]
bj = random.choice(bi_list)
bk = random.choice(bj)
print(bk)