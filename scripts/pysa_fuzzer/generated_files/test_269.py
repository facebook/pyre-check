import random
import math

a = input()
b_list = [a for _ in range(7)]
c_list = [b_list for _ in range(8)]
d_list = [c_list for _ in range(2)]
e = random.choice(d_list)
f = random.choice(e)
g = random.choice(f)
h = (g, g, g)
i, j, k = h
l = i + j + k
m = l + '6'
n = m + '2'
o_dict = {28: n, 42: n, 59: n, 74: n, 58: n}
p = random.choice(list(o_dict.values()))
q = ''
for _ in range(2):
    for __ in range(4):
                q += p
r = q[0:]
s_list = [r for _ in range(7)]
t = random.choice(s_list)
u = ''
for _ in range(5):
        if _ == 4:
            continue
        u += t
if u == u:
    x = u + 'c1'
elif u == '12':
    x = v + 'c2'
else:
    x = w + 'c3'
y = x + '3'
z_dict = {55: y, 30: y, 8: y, 84: y, 66: y, 77: y, 94: y, 51: y}
aa = random.choice(list(z_dict.values()))
ab = ''
for _ in range(5):
        if _ == 4:
            break
        ab += aa
ac = ''
for _ in range(3):
    for __ in range(5):
                ac += ab
ad = ''
counterad = 0
while counterad < 5:
    ae = ''
    counterae = 0
    while counterae < 4:
        af = ''
        counteraf = 0
        while counteraf < 2:
            af += ae
            counteraf += 1
            ae += ad
            counterae += 1
        ad += ac
        counterad += 1
ag = f'string {af}'
ah = ''
for _ in range(3):
    ai = ''
    for _ in range(2):
        aj = ''
        for _ in range(4):
            aj += ai
            ai += ah
        ah += ag
ak_dict = {45: aj, 83: aj, 18: aj, 68: aj, 99: aj}
al = random.choice(list(ak_dict.values()))
am = (al, al, al)
an, ao, ap = am
aq = an + ao + ap
print(aq)