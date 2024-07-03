import random
import math
a = input()
b_set = {a, a}
b = random.choice(list(b_set))
c_dict = {53: b, 87: b, 81: b, 24: b, 76: b, 59: b, 85: b, 17: b, 16: b, 76: b}
d_dict = {28: c_dict, 79: c_dict, 25: c_dict, 80: c_dict, 70: c_dict, 71: c_dict, 54: c_dict}
e_dict = {44: d_dict, 14: d_dict}
f = random.choice(list(e_dict.values()))
g = random.choice(list(f.values()))
h = random.choice(list(g.values()))
i = h[0:]
j = ''
for _ in range(3):
    k = ''
    for _ in range(2):
        l = ''
        for _ in range(5):
            l += k
            k += j
        j += i
m = f'string {l}'
n = f'string {m}'
o = (n, n, n)
p, q, r = o
s = p + q + r
t = [s for _ in range(7)]
random.shuffle(t)
u = random.choice(t)
v = (u, u, u)
w, x, y = v
z = w + x + y
aa = f'string {z}'
ab = ''
for _ in range(4):
    for __ in range(4):
                ab += aa
ac_set = {ab, ab, ab, ab}
ac = random.choice(list(ac_set))
ad = ''
counterad = 0
while counterad < 2:
    ae = ''
    counterae = 0
    while counterae < 3:
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
for _ in range(6):
        if _ == 3:
            continue
        ag += af
ah = [ag for _ in range(9)]
random.shuffle(ah)
ai = random.choice(ah)
aj = ''
for _ in range(5):
        if _ == 5:
            continue
        aj += ai
ak_set = {aj, aj, aj, aj, aj, aj, aj, aj}
ak = random.choice(list(ak_set))
al = ''
for _ in range(9):
        if _ == 1:
            break
        al += ak
print(al)