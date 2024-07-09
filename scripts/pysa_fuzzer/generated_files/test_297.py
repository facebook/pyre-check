import random
import math

a = input()
b = [a for _ in range(10)]
random.shuffle(b)
c = random.choice(b)
d = f'string {c}'
e = ''
countere = 0
while countere < 3:
    f = ''
    counterf = 0
    while counterf < 3:
        g = ''
        counterg = 0
        while counterg < 2:
            g += f
            counterg += 1
            f += e
            counterf += 1
        e += d
        countere += 1
h_dict = {16: g, 11: g, 78: g, 33: g, 74: g, 38: g, 93: g, 60: g, 31: g, 67: g}
i_dict = {66: h_dict, 67: h_dict, 26: h_dict, 83: h_dict, 23: h_dict, 55: h_dict, 25: h_dict}
j_dict = {25: i_dict, 8: i_dict, 68: i_dict, 52: i_dict, 41: i_dict, 100: i_dict, 76: i_dict}
k = random.choice(list(j_dict.values()))
l = random.choice(list(k.values()))
m = random.choice(list(l.values()))
n_set = {m, m}
n = random.choice(list(n_set))
o = ''
for _ in range(5):
        if _ == 3:
            break
        o += n
p = ''
for _ in range(4):
    for __ in range(5):
                p += o
q_dict = {66: p, 94: p}
r_dict = {10: q_dict, 83: q_dict, 30: q_dict, 91: q_dict}
s = random.choice(list(r_dict.values()))
t = random.choice(list(s.values()))
def u():
    return t
v = u()
w = (v, v, v)
x, y, z = w
aa = x + y + z
ab = aa + '5'
ac = ab + '3'
ad = (ac, ac, ac)
ae, af, ag = ad
ah = ae + af + ag
ai = f'string {ah}'
aj = f'string {ai}'
ak = ''
for _ in range(8):
        if _ == 1:
            break
        ak += aj
al = [ak for _ in range(7)]
random.shuffle(al)
am = random.choice(al)
an = ''
for _ in range(7):
        if _ == 5:
            break
        an += am
ao = ''
counterao = 0
while counterao < 5:
    ao += an
    counterao += 1
print(ao)