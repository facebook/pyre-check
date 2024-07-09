import random
import math

a = input()
b_dict = {94: a, 70: a, 85: a, 100: a, 4: a, 64: a, 16: a, 3: a, 43: a, 5: a}
c_dict = {14: b_dict, 88: b_dict, 99: b_dict}
d_dict = {62: c_dict, 81: c_dict, 11: c_dict, 31: c_dict, 33: c_dict, 25: c_dict, 10: c_dict, 68: c_dict, 48: c_dict}
e = random.choice(list(d_dict.values()))
f = random.choice(list(e.values()))
g = random.choice(list(f.values()))
h = ''
for _ in range(8):
        if _ == 3:
            break
        h += g
i = h + '6'
j = i + '3'
k = ''
for _ in range(3):
    l = ''
    for _ in range(2):
        m = ''
        for _ in range(4):
            m += l
            l += k
        k += j
n_set = {m, m, m, m, m}
n = random.choice(list(n_set))
o = ''
for _ in range(9):
        if _ == 3:
            break
        o += n
def p():
    return o
def q():
    return p()
r = q()
s = ''
for _ in range(7):
        if _ == 5:
            continue
        s += r
t = s + '7'
u = t + '6'
v = u + '2'
w = (v, v, v)
x, y, z = w
aa = x + y + z
ab_list = [aa for _ in range(5)]
ac_list = [ab_list for _ in range(2)]
ad_list = [ac_list for _ in range(3)]
ae = random.choice(ad_list)
af = random.choice(ae)
ag = random.choice(af)
ah = (ag, ag, ag)
ai, aj, ak = ah
al = ai + aj + ak
am = ''
for _ in range(6):
        if _ == 4:
            continue
        am += al
an_list = [am for _ in range(2)]
ao_list = [an_list for _ in range(2)]
ap_list = [ao_list for _ in range(7)]
aq = random.choice(ap_list)
ar = random.choice(aq)
at = random.choice(ar)
au = at + '.'
av = ''
for _ in range(3):
    for __ in range(5):
                av += au
aw = av + '5'
ax = aw + '3'
ay = ''
for _ in range(3):
    for __ in range(2):
                ay += ax
print(ay)