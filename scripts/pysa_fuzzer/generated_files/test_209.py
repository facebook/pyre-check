import random
import math

a = input()
b_list = [a for _ in range(2)]
c_list = [b_list for _ in range(3)]
d_list = [c_list for _ in range(6)]
e = random.choice(d_list)
f = random.choice(e)
g = random.choice(f)
h_set = {g, g, g, g, g, g}
h = random.choice(list(h_set))
i_list = [h for _ in range(2)]
j_list = [i_list for _ in range(5)]
k_list = [j_list for _ in range(3)]
l = random.choice(k_list)
m = random.choice(l)
n = random.choice(m)
o = (n, n, n)
p, q, r = o
s = p + q + r
t = ''
for _ in range(7):
        if _ == 3:
            break
        t += s
u = t[0:]
v = f'string {u}'
w = ''
for _ in range(3):
    x = ''
    for _ in range(3):
        x += w
        w += v
if x == x:
    aa = x + 'c1'
elif x == '15':
    aa = y + 'c2'
else:
    aa = z + 'c3'
ab_list = [aa for _ in range(3)]
ac_list = [ab_list for _ in range(3)]
ad = random.choice(ac_list)
ae = random.choice(ad)
af = f'string {ae}'
ag = ''
for _ in range(7):
        if _ == 5:
            continue
        ag += af
ah = ''
for _ in range(5):
    for __ in range(2):
                ah += ag
ai = ''
counterai = 0
while counterai < 2:
    aj = ''
    counteraj = 0
    while counteraj < 4:
        aj += ai
        counteraj += 1
        ai += ah
        counterai += 1
ak_dict = {2: aj, 38: aj, 56: aj, 47: aj, 33: aj, 2: aj}
al = random.choice(list(ak_dict.values()))
am = al + '4'
an = am + '5'
ao = an + '4'
if ao == ao:
    ar = ao + 'c1'
elif ao == '20':
    ar = ap + 'c2'
else:
    ar = aq + 'c3'
at_set = {ar, ar, ar, ar, ar, ar, ar, ar}
at = random.choice(list(at_set))
print(at)