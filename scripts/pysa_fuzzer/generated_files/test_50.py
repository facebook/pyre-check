import random
import math

a = input()
b_dict = {89: a, 77: a}
c_dict = {10: b_dict, 21: b_dict, 65: b_dict, 49: b_dict, 18: b_dict, 1: b_dict}
d = random.choice(list(c_dict.values()))
e = random.choice(list(d.values()))
f = ''
counterf = 0
while counterf < 4:
    g = ''
    counterg = 0
    while counterg < 4:
        h = ''
        counterh = 0
        while counterh < 2:
            h += g
            counterh += 1
            g += f
            counterg += 1
        f += e
        counterf += 1
i_dict = {53: h, 96: h}
j_dict = {3: i_dict, 91: i_dict, 28: i_dict, 37: i_dict}
k_dict = {96: j_dict, 75: j_dict, 55: j_dict}
l = random.choice(list(k_dict.values()))
m = random.choice(list(l.values()))
n = random.choice(list(m.values()))
o = ''
countero = 0
while countero < 2:
    p = ''
    counterp = 0
    while counterp < 2:
        p += o
        counterp += 1
        o += n
        countero += 1
q = p[0:]
r = q + '.'
s = f'string {r}'
t_set = {s, s, s, s, s, s, s, s, s, s}
t = random.choice(list(t_set))
u_set = {t, t, t, t, t, t, t, t, t, t}
u = random.choice(list(u_set))
if u == u:
    x = u + 'c1'
elif u == '15':
    x = v + 'c2'
else:
    x = w + 'c3'
y = ''
for _ in range(2):
    for __ in range(5):
                y += x
z = y[0:]
aa = f'string {z}'
def ab():
    return aa
def ac():
    return ab()
def ad():
    return ac()
ae = ad()
af = f'string {ae}'
ag_list = [af for _ in range(9)]
ah = random.choice(ag_list)
if ah == ah:
    ak = ah + 'c1'
elif ah == '16':
    ak = ai + 'c2'
else:
    ak = aj + 'c3'
al = f'string {ak}'
print(al)