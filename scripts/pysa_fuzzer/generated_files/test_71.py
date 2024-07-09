import random
import math

a = input()
b_list = [a for _ in range(7)]
c_list = [b_list for _ in range(6)]
d = random.choice(c_list)
e = random.choice(d)
f = [e for _ in range(7)]
random.shuffle(f)
g = random.choice(f)
h = g + '7'
i = h + '8'
j = i + '2'
k = j + '6'
l = k + '6'
m = l + '2'
n = ''
for _ in range(3):
    for __ in range(3):
                n += m
o = n + '6'
p = o + '7'
q = ''
for _ in range(3):
    r = ''
    for _ in range(4):
        r += q
        q += p
if r == r:
    u = r + 'c1'
elif r == '11':
    u = s + 'c2'
else:
    u = t + 'c3'
v = f'string {u}'
w = ''
counterw = 0
while counterw < 2:
    x = ''
    counterx = 0
    while counterx < 4:
        y = ''
        countery = 0
        while countery < 4:
            y += x
            countery += 1
            x += w
            counterx += 1
        w += v
        counterw += 1
z = ''
for _ in range(9):
        if _ == 1:
            break
        z += y
aa = ''
for _ in range(3):
    ab = ''
    for _ in range(2):
        ab += aa
        aa += z
ac = ab[0:]
ad = ''
counterad = 0
while counterad < 4:
    ae = ''
    counterae = 0
    while counterae < 5:
        af = ''
        counteraf = 0
        while counteraf < 5:
            af += ae
            counteraf += 1
            ae += ad
            counterae += 1
        ad += ac
        counterad += 1
ag = ''
counterag = 0
while counterag < 2:
    ah = ''
    counterah = 0
    while counterah < 4:
        ai = ''
        counterai = 0
        while counterai < 5:
            ai += ah
            counterai += 1
            ah += ag
            counterah += 1
        ag += af
        counterag += 1
aj = ''
for _ in range(10):
        if _ == 2:
            break
        aj += ai
ak_dict = {69: aj, 4: aj, 4: aj}
al_dict = {74: ak_dict, 75: ak_dict}
am_dict = {7: al_dict, 88: al_dict, 92: al_dict, 80: al_dict, 5: al_dict, 28: al_dict}
an = random.choice(list(am_dict.values()))
ao = random.choice(list(an.values()))
ap = random.choice(list(ao.values()))
aq_dict = {41: ap, 90: ap, 54: ap, 38: ap, 31: ap, 46: ap, 56: ap, 42: ap, 13: ap}
ar_dict = {76: aq_dict, 27: aq_dict, 3: aq_dict, 91: aq_dict, 74: aq_dict, 92: aq_dict, 25: aq_dict}
at = random.choice(list(ar_dict.values()))
au = random.choice(list(at.values()))
print(au)