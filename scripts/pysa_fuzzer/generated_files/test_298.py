import random
import math

a = input()
b = ''
for _ in range(2):
    for __ in range(2):
                b += a
c = ''
counterc = 0
while counterc < 3:
    d = ''
    counterd = 0
    while counterd < 2:
        e = ''
        countere = 0
        while countere < 5:
            e += d
            countere += 1
            d += c
            counterd += 1
        c += b
        counterc += 1
f_dict = {84: e, 39: e, 72: e, 57: e}
g_dict = {40: f_dict, 86: f_dict, 60: f_dict, 84: f_dict}
h_dict = {26: g_dict, 11: g_dict, 38: g_dict}
i = random.choice(list(h_dict.values()))
j = random.choice(list(i.values()))
k = random.choice(list(j.values()))
l = k + '1'
m = l + '7'
n_dict = {21: m, 30: m, 11: m, 71: m, 71: m}
o_dict = {52: n_dict, 71: n_dict}
p_dict = {78: o_dict, 57: o_dict, 74: o_dict, 5: o_dict, 3: o_dict, 18: o_dict}
q = random.choice(list(p_dict.values()))
r = random.choice(list(q.values()))
s = random.choice(list(r.values()))
t = (s, s, s)
u, v, w = t
x = u + v + w
y_list = [x for _ in range(10)]
z_list = [y_list for _ in range(6)]
aa = random.choice(z_list)
ab = random.choice(aa)
ac = f'string {ab}'
if ac == ac:
    af = ac + 'c1'
elif ac == '11':
    af = ad + 'c2'
else:
    af = ae + 'c3'
ag = ''
for _ in range(5):
        if _ == 5:
            continue
        ag += af
if ag == ag:
    aj = ag + 'c1'
elif ag == '19':
    aj = ah + 'c2'
else:
    aj = ai + 'c3'
ak = ''
for _ in range(2):
    ak += aj
al = (ak, ak, ak)
am, an, ao = al
ap = am + an + ao
aq = ''
for _ in range(8):
        if _ == 1:
            break
        aq += ap
ar = [aq for _ in range(10)]
random.shuffle(ar)
at = random.choice(ar)
au = at + '.'
av = [au for _ in range(7)]
random.shuffle(av)
aw = random.choice(av)
ax_set = {aw, aw, aw, aw, aw, aw, aw}
ax = random.choice(list(ax_set))
print(ax)