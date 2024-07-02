import random
import math
a = input()
b = a + '3'
c = b + '8'
d = c + '5'
e = ''
for _ in range(5):
    for __ in range(4):
                e += d
f_dict = {84: e, 80: e, 43: e, 63: e, 74: e}
g_dict = {98: f_dict, 56: f_dict, 26: f_dict, 97: f_dict, 60: f_dict, 27: f_dict, 71: f_dict, 25: f_dict, 66: f_dict}
h_dict = {49: g_dict, 14: g_dict}
i = random.choice(list(h_dict.values()))
j = random.choice(list(i.values()))
k = random.choice(list(j.values()))
l_dict = {24: k, 42: k, 84: k, 55: k, 12: k, 61: k, 20: k, 18: k, 83: k, 35: k}
m = random.choice(list(l_dict.values()))
n = [m for _ in range(9)]
random.shuffle(n)
o = random.choice(n)
p = ''
counterp = 0
while counterp < 2:
    q = ''
    counterq = 0
    while counterq < 4:
        q += p
        counterq += 1
        p += o
        counterp += 1
r_list = [q for _ in range(6)]
s = random.choice(r_list)
t_list = [s for _ in range(3)]
u = random.choice(t_list)
v = u + '.'
w = (v, v, v)
x, y, z = w
aa = x + y + z
if aa == '5':
    ab = aa + ' c1'
elif aa == '11':
    ab = aa + ' c2'
else:
    ab = aa + ' c3'
ac = [ab for _ in range(8)]
random.shuffle(ac)
ad = random.choice(ac)
ae = ad + '2'
af = ae + '8'
ag = af + '9'
ah = ''
for _ in range(5):
    ai = ''
    for _ in range(5):
        ai += ah
        ah += ag
aj = (ai, ai, ai)
ak, al, am = aj
an = ak + al + am
ao = ''
for _ in range(3):
    for __ in range(5):
                ao += an
ap = ao + '.'
aq = ''
for _ in range(9):
        if _ == 3:
            continue
        aq += ap
print(aq)