import random
import math

a = input()
b_dict = {40: a, 100: a, 41: a, 79: a, 48: a, 52: a, 33: a, 56: a}
c = random.choice(list(b_dict.values()))
d = ''
for _ in range(3):
    for __ in range(3):
                d += c
e = ''
countere = 0
while countere < 5:
    f = ''
    counterf = 0
    while counterf < 5:
        g = ''
        counterg = 0
        while counterg < 3:
            g += f
            counterg += 1
            f += e
            counterf += 1
        e += d
        countere += 1
h = ''
for _ in range(10):
        if _ == 4:
            continue
        h += g
i_dict = {28: h, 63: h, 77: h, 6: h, 66: h, 73: h, 43: h, 3: h, 63: h}
j_dict = {20: i_dict, 47: i_dict, 73: i_dict}
k_dict = {22: j_dict, 11: j_dict, 1: j_dict, 43: j_dict, 64: j_dict, 18: j_dict, 42: j_dict, 76: j_dict, 4: j_dict}
l = random.choice(list(k_dict.values()))
m = random.choice(list(l.values()))
n = random.choice(list(m.values()))
o = [n for _ in range(7)]
random.shuffle(o)
p = random.choice(o)
q = p + '.'
def r():
    return q
s = r()
t = s + '.'
u = t[0:]
v = ''
for _ in range(4):
    for __ in range(2):
                v += u
w = v[0:]
x = ''
for _ in range(4):
    for __ in range(2):
                x += w
y_dict = {30: x, 92: x, 38: x, 54: x, 30: x, 64: x, 72: x, 57: x, 55: x}
z_dict = {42: y_dict, 64: y_dict, 78: y_dict}
aa_dict = {22: z_dict, 53: z_dict, 40: z_dict, 9: z_dict, 44: z_dict, 77: z_dict}
ab = random.choice(list(aa_dict.values()))
ac = random.choice(list(ab.values()))
ad = random.choice(list(ac.values()))
ae = f'string {ad}'
af = ae + '1'
ag = af + '4'
ah = ag + '3'
ai = ''
for _ in range(7):
        if _ == 2:
            continue
        ai += ah
if ai == ai:
    al = ai + 'c1'
elif ai == '20':
    al = aj + 'c2'
else:
    al = ak + 'c3'
print(al)