import random
import math

a = input()
b = ''
for _ in range(2):
    for __ in range(3):
                b += a
c = ''
for _ in range(3):
    c += b
d = ''
counterd = 0
while counterd < 5:
    d += c
    counterd += 1
e_dict = {8: d, 92: d, 74: d, 67: d}
f_dict = {52: e_dict, 86: e_dict, 15: e_dict, 83: e_dict, 65: e_dict, 35: e_dict, 88: e_dict}
g_dict = {32: f_dict, 58: f_dict, 4: f_dict, 32: f_dict, 84: f_dict, 55: f_dict, 68: f_dict, 78: f_dict}
h = random.choice(list(g_dict.values()))
i = random.choice(list(h.values()))
j = random.choice(list(i.values()))
def k():
    return j
def l():
    return k()
m = l()
n = m + '.'
o_dict = {67: n, 33: n, 61: n, 98: n, 4: n, 41: n, 15: n, 18: n, 85: n, 78: n}
p_dict = {91: o_dict, 48: o_dict, 83: o_dict, 17: o_dict, 35: o_dict, 22: o_dict, 51: o_dict, 49: o_dict, 11: o_dict}
q = random.choice(list(p_dict.values()))
r = random.choice(list(q.values()))
s_dict = {61: r, 34: r, 98: r, 93: r, 81: r, 16: r, 58: r}
t_dict = {82: s_dict, 29: s_dict, 54: s_dict, 58: s_dict, 88: s_dict, 7: s_dict, 55: s_dict}
u_dict = {74: t_dict, 79: t_dict, 92: t_dict, 22: t_dict, 16: t_dict, 18: t_dict}
v = random.choice(list(u_dict.values()))
w = random.choice(list(v.values()))
x = random.choice(list(w.values()))
y_dict = {89: x, 80: x, 66: x, 62: x, 84: x, 78: x}
z_dict = {83: y_dict, 54: y_dict, 6: y_dict, 46: y_dict, 43: y_dict, 28: y_dict}
aa = random.choice(list(z_dict.values()))
ab = random.choice(list(aa.values()))
ac = f'string {ab}'
ad = [ac for _ in range(10)]
random.shuffle(ad)
ae = random.choice(ad)
if ae == ae:
    ah = ae + 'c1'
elif ae == '12':
    ah = af + 'c2'
else:
    ah = ag + 'c3'
ai = [ah for _ in range(5)]
random.shuffle(ai)
aj = random.choice(ai)
ak = ''
counterak = 0
while counterak < 4:
    al = ''
    counteral = 0
    while counteral < 2:
        am = ''
        counteram = 0
        while counteram < 3:
            am += al
            counteram += 1
            al += ak
            counteral += 1
        ak += aj
        counterak += 1
an = ''
counteran = 0
while counteran < 4:
    ao = ''
    counterao = 0
    while counterao < 3:
        ao += an
        counterao += 1
        an += am
        counteran += 1
ap = ''
for _ in range(4):
    aq = ''
    for _ in range(3):
        ar = ''
        for _ in range(2):
            ar += aq
            aq += ap
        ap += ao
def at():
    return ar
def au():
    return at()
def av():
    return au()
aw = av()
ax = ''
for _ in range(3):
    for __ in range(4):
                ax += aw
print(ax)