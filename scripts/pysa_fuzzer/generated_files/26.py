import random
import math
a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g = ''
counterg = 0
while counterg < 3:
    h = ''
    counterh = 0
    while counterh < 3:
        h += g
        counterh += 1
        g += f
        counterg += 1
i = ''
for _ in range(2):
    for __ in range(2):
                i += h
j = ''
for _ in range(3):
    for __ in range(4):
                j += i
if j == '10':
    k = j + ' c1'
elif j == '11':
    k = j + ' c2'
else:
    k = j + ' c3'
l = ''
for _ in range(3):
    for __ in range(3):
                l += k
m_set = {l, l, l, l}
m = random.choice(list(m_set))
n_dict = {57: m, 39: m, 26: m, 97: m, 95: m, 3: m}
o_dict = {67: n_dict, 58: n_dict, 66: n_dict, 68: n_dict, 14: n_dict, 14: n_dict, 2: n_dict}
p = random.choice(list(o_dict.values()))
q = random.choice(list(p.values()))
r_list = [q for _ in range(8)]
s_list = [r_list for _ in range(5)]
t_list = [s_list for _ in range(8)]
u = random.choice(t_list)
v = random.choice(u)
w = random.choice(v)
x = [w for _ in range(8)]
random.shuffle(x)
y = random.choice(x)
z = ''
counterz = 0
while counterz < 5:
    aa = ''
    counteraa = 0
    while counteraa < 2:
        ab = ''
        counterab = 0
        while counterab < 4:
            ab += aa
            counterab += 1
            aa += z
            counteraa += 1
        z += y
        counterz += 1
ac = [ab for _ in range(10)]
random.shuffle(ac)
ad = random.choice(ac)
ae = (ad, ad, ad)
af, ag, ah = ae
ai = af + ag + ah
aj = ai + '.'
ak = ''
for _ in range(4):
    ak += aj
al = ''
for _ in range(3):
    al += ak
am = [al for _ in range(8)]
random.shuffle(am)
an = random.choice(am)
ao = an[0:]
print(ao)