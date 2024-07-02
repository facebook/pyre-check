import random
import math
a = input()
b_list = [a for _ in range(6)]
c_list = [b_list for _ in range(6)]
d_list = [c_list for _ in range(4)]
e = random.choice(d_list)
f = random.choice(e)
g = random.choice(f)
h = [g for _ in range(6)]
random.shuffle(h)
i = random.choice(h)
j = (i, i, i)
k, l, m = j
n = k + l + m
o_set = {n, n, n, n, n, n}
o = random.choice(list(o_set))
def p():
    return o
def q():
    return p()
def r():
    return q()
s = r()
t = [s for _ in range(6)]
random.shuffle(t)
u = random.choice(t)
v = u + '4'
w = v + '3'
x = ''
for _ in range(8):
        if _ == 3:
            break
        x += w
y = ''
for _ in range(3):
    z = ''
    for _ in range(3):
        z += y
        y += x
aa_set = {z, z, z, z, z, z, z, z, z}
aa = random.choice(list(aa_set))
ab_dict = {33: aa, 75: aa, 62: aa, 94: aa, 57: aa}
ac_dict = {21: ab_dict, 30: ab_dict, 50: ab_dict, 18: ab_dict, 100: ab_dict, 55: ab_dict, 44: ab_dict, 94: ab_dict, 82: ab_dict}
ad_dict = {29: ac_dict, 22: ac_dict, 21: ac_dict, 66: ac_dict, 74: ac_dict, 78: ac_dict, 76: ac_dict, 6: ac_dict, 12: ac_dict}
ae = random.choice(list(ad_dict.values()))
af = random.choice(list(ae.values()))
ag = random.choice(list(af.values()))
ah = (ag, ag, ag)
ai, aj, ak = ah
al = ai + aj + ak
am_list = [al for _ in range(2)]
an_list = [am_list for _ in range(8)]
ao = random.choice(an_list)
ap = random.choice(ao)
aq = ''
counteraq = 0
while counteraq < 2:
    aq += ap
    counteraq += 1
ar_list = [aq for _ in range(2)]
at_list = [ar_list for _ in range(6)]
au = random.choice(at_list)
av = random.choice(au)
aw_set = {av, av, av, av}
aw = random.choice(list(aw_set))
ax = ''
for _ in range(7):
        if _ == 5:
            break
        ax += aw
ay = ''
for _ in range(4):
    for __ in range(2):
                ay += ax
print(ay)