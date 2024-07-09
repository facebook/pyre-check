import random
import math

a = input()
b = ''
counterb = 0
while counterb < 2:
    c = ''
    counterc = 0
    while counterc < 2:
        d = ''
        counterd = 0
        while counterd < 3:
            d += c
            counterd += 1
            c += b
            counterc += 1
        b += a
        counterb += 1
e = d + '7'
f = e + '3'
g = ''
counterg = 0
while counterg < 5:
    g += f
    counterg += 1
h = [g for _ in range(9)]
random.shuffle(h)
i = random.choice(h)
j = [i for _ in range(10)]
random.shuffle(j)
k = random.choice(j)
l = ''
for _ in range(5):
    m = ''
    for _ in range(3):
        m += l
        l += k
n_list = [m for _ in range(5)]
o_list = [n_list for _ in range(10)]
p = random.choice(o_list)
q = random.choice(p)
r = q + '5'
s_dict = {66: r, 33: r}
t_dict = {15: s_dict, 70: s_dict, 98: s_dict, 56: s_dict}
u_dict = {46: t_dict, 35: t_dict, 12: t_dict, 12: t_dict, 45: t_dict, 41: t_dict, 65: t_dict}
v = random.choice(list(u_dict.values()))
w = random.choice(list(v.values()))
x = random.choice(list(w.values()))
y = ''
for _ in range(5):
        if _ == 2:
            continue
        y += x
z = ''
for _ in range(3):
    aa = ''
    for _ in range(5):
        aa += z
        z += y
ab = ''
counterab = 0
while counterab < 4:
    ac = ''
    counterac = 0
    while counterac < 3:
        ad = ''
        counterad = 0
        while counterad < 3:
            ad += ac
            counterad += 1
            ac += ab
            counterac += 1
        ab += aa
        counterab += 1
ae = ad + '3'
af = ae + '8'
ag = ''
for _ in range(4):
    ah = ''
    for _ in range(2):
        ah += ag
        ag += af
ai = [ah for _ in range(6)]
random.shuffle(ai)
aj = random.choice(ai)
ak_set = {aj, aj, aj}
ak = random.choice(list(ak_set))
al = ak + '.'
am = al + '1'
print(am)