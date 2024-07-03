import random
import math
a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g = ''
counterg = 0
while counterg < 2:
    g += f
    counterg += 1
h = ''
for _ in range(6):
        if _ == 4:
            continue
        h += g
i = ''
for _ in range(3):
    for __ in range(4):
                i += h
j = ''
counterj = 0
while counterj < 4:
    j += i
    counterj += 1
k = (j, j, j)
l, m, n = k
o = l + m + n
if o == '2':
    p = o + ' c1'
elif o == '11':
    p = o + ' c2'
else:
    p = o + ' c3'
q_dict = {28: p, 84: p, 49: p, 48: p}
r = random.choice(list(q_dict.values()))
s_dict = {84: r, 85: r, 83: r, 1: r, 82: r, 42: r, 93: r, 20: r}
t_dict = {48: s_dict, 24: s_dict, 26: s_dict, 12: s_dict, 36: s_dict, 26: s_dict, 90: s_dict, 47: s_dict, 32: s_dict}
u_dict = {95: t_dict, 22: t_dict, 61: t_dict}
v = random.choice(list(u_dict.values()))
w = random.choice(list(v.values()))
x = random.choice(list(w.values()))
y_set = {x, x}
y = random.choice(list(y_set))
z = ''
for _ in range(5):
    for __ in range(2):
                z += y
def aa():
    return z
def ab():
    return aa()
ac = ab()
ad = ac + '4'
ae = ad[0:]
af_list = [ae for _ in range(9)]
ag_list = [af_list for _ in range(6)]
ah = random.choice(ag_list)
ai = random.choice(ah)
aj = ''
counteraj = 0
while counteraj < 4:
    aj += ai
    counteraj += 1
ak_dict = {76: aj, 43: aj, 51: aj, 45: aj, 20: aj}
al = random.choice(list(ak_dict.values()))
am = al + '.'
print(am)