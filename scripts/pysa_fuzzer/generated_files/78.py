import random
import math
a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g = ''
for _ in range(4):
    g += f
h = ''
counterh = 0
while counterh < 3:
    h += g
    counterh += 1
i = h + '.'
j = i + '9'
k = j + '7'
l = k + '8'
m = l + '9'
n_dict = {22: m, 22: m, 76: m}
o_dict = {62: n_dict, 52: n_dict, 13: n_dict, 39: n_dict, 67: n_dict, 55: n_dict, 56: n_dict, 52: n_dict}
p = random.choice(list(o_dict.values()))
q = random.choice(list(p.values()))
if q == '1':
    r = q + ' c1'
elif q == '12':
    r = q + ' c2'
else:
    r = q + ' c3'
s_dict = {76: r, 46: r, 5: r, 62: r}
t_dict = {57: s_dict, 26: s_dict}
u = random.choice(list(t_dict.values()))
v = random.choice(list(u.values()))
w_list = [v for _ in range(2)]
x_list = [w_list for _ in range(6)]
y_list = [x_list for _ in range(10)]
z = random.choice(y_list)
aa = random.choice(z)
ab = random.choice(aa)
ac = (ab, ab, ab)
ad, ae, af = ac
ag = ad + ae + af
ah = ''
for _ in range(6):
        if _ == 3:
            break
        ah += ag
ai = ''
for _ in range(9):
        if _ == 3:
            break
        ai += ah
aj = ''
counteraj = 0
while counteraj < 2:
    ak = ''
    counterak = 0
    while counterak < 4:
        al = ''
        counteral = 0
        while counteral < 5:
            al += ak
            counteral += 1
            ak += aj
            counterak += 1
        aj += ai
        counteraj += 1
am_list = [al for _ in range(6)]
an_list = [am_list for _ in range(8)]
ao = random.choice(an_list)
ap = random.choice(ao)
aq = ap[0:]
ar_dict = {88: aq, 39: aq, 87: aq, 65: aq, 46: aq}
at = random.choice(list(ar_dict.values()))
au_list = [at for _ in range(2)]
av_list = [au_list for _ in range(5)]
aw_list = [av_list for _ in range(4)]
ax = random.choice(aw_list)
ay = random.choice(ax)
az = random.choice(ay)
print(az)