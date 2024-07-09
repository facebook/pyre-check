import random
import math

a = input()
b = [a for _ in range(9)]
random.shuffle(b)
c = random.choice(b)
d = c[0:]
e = ''
for _ in range(6):
        if _ == 3:
            break
        e += d
f = ''
counterf = 0
while counterf < 4:
    f += e
    counterf += 1
g_dict = {32: f, 78: f, 42: f, 39: f, 94: f, 4: f, 49: f, 79: f, 13: f, 24: f}
h_dict = {89: g_dict, 45: g_dict, 52: g_dict, 56: g_dict, 88: g_dict, 36: g_dict, 91: g_dict, 69: g_dict, 15: g_dict, 90: g_dict}
i_dict = {65: h_dict, 80: h_dict, 39: h_dict, 68: h_dict, 2: h_dict, 25: h_dict}
j = random.choice(list(i_dict.values()))
k = random.choice(list(j.values()))
l = random.choice(list(k.values()))
m_set = {l, l, l, l, l, l, l, l, l}
m = random.choice(list(m_set))
if m == m:
    p = m + 'c1'
elif m == '20':
    p = n + 'c2'
else:
    p = o + 'c3'
q = p + '8'
r = q + '5'
s = (r, r, r)
t, u, v = s
w = t + u + v
x_set = {w, w, w}
x = random.choice(list(x_set))
y_dict = {66: x, 37: x, 21: x, 74: x, 7: x, 4: x, 53: x}
z = random.choice(list(y_dict.values()))
aa = ''
counteraa = 0
while counteraa < 5:
    ab = ''
    counterab = 0
    while counterab < 2:
        ab += aa
        counterab += 1
        aa += z
        counteraa += 1
ac = f'string {ab}'
if ac == ac:
    af = ac + 'c1'
elif ac == '18':
    af = ad + 'c2'
else:
    af = ae + 'c3'
ag_set = {af, af, af, af, af, af, af}
ag = random.choice(list(ag_set))
ah = ''
for _ in range(9):
        if _ == 4:
            continue
        ah += ag
ai_list = [ah for _ in range(8)]
aj = random.choice(ai_list)
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
print(al)