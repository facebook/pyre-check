import random
import math

a = input()
b = a + '8'
c = b + '7'
d_dict = {79: c, 54: c, 89: c, 28: c}
e_dict = {60: d_dict, 55: d_dict, 23: d_dict, 10: d_dict, 19: d_dict, 68: d_dict}
f_dict = {74: e_dict, 75: e_dict, 37: e_dict, 31: e_dict, 62: e_dict, 95: e_dict, 58: e_dict}
g = random.choice(list(f_dict.values()))
h = random.choice(list(g.values()))
i = random.choice(list(h.values()))
j = (i, i, i)
k, l, m = j
n = k + l + m
o = [n for _ in range(8)]
random.shuffle(o)
p = random.choice(o)
q = f'string {p}'
r_list = [q for _ in range(7)]
s_list = [r_list for _ in range(5)]
t_list = [s_list for _ in range(6)]
u = random.choice(t_list)
v = random.choice(u)
w = random.choice(v)
def x():
    return w
y = x()
z_list = [y for _ in range(10)]
aa_list = [z_list for _ in range(9)]
ab_list = [aa_list for _ in range(5)]
ac = random.choice(ab_list)
ad = random.choice(ac)
ae = random.choice(ad)
def af():
    return ae
def ag():
    return af()
ah = ag()
ai = f'string {ah}'
def aj():
    return ai
ak = aj()
al = ''
for _ in range(4):
    am = ''
    for _ in range(4):
        am += al
        al += ak
an = f'string {am}'
if an == an:
    aq = an + 'c1'
elif an == '12':
    aq = ao + 'c2'
else:
    aq = ap + 'c3'
ar = ''
for _ in range(3):
    ar += aq
at = ar + '2'
au = at + '6'
av = ''
counterav = 0
while counterav < 2:
    aw = ''
    counteraw = 0
    while counteraw < 2:
        aw += av
        counteraw += 1
        av += au
        counterav += 1
ax = [aw for _ in range(9)]
random.shuffle(ax)
ay = random.choice(ax)
print(ay)