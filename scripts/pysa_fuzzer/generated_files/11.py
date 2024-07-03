import random
import math
a = input()
b_dict = {76: a, 58: a, 12: a, 74: a, 3: a, 88: a, 97: a, 12: a, 35: a}
c_dict = {12: b_dict, 92: b_dict, 39: b_dict}
d_dict = {32: c_dict, 34: c_dict, 86: c_dict, 82: c_dict, 74: c_dict, 40: c_dict, 72: c_dict, 99: c_dict}
e = random.choice(list(d_dict.values()))
f = random.choice(list(e.values()))
g = random.choice(list(f.values()))
h = (g, g, g)
i, j, k = h
l = i + j + k
m = ''
for _ in range(8):
        if _ == 2:
            break
        m += l
n = ''
for _ in range(5):
    for __ in range(5):
                n += m
o = [n for _ in range(7)]
random.shuffle(o)
p = random.choice(o)
q = p[0:]
r = ''
for _ in range(3):
    for __ in range(5):
                r += q
s = r[0:]
t = f'string {s}'
u_set = {t, t}
u = random.choice(list(u_set))
v = ''
counterv = 0
while counterv < 2:
    w = ''
    counterw = 0
    while counterw < 2:
        x = ''
        counterx = 0
        while counterx < 3:
            x += w
            counterx += 1
            w += v
            counterw += 1
        v += u
        counterv += 1
y = ''
for _ in range(4):
    for __ in range(3):
                y += x
z = ''
for _ in range(3):
    aa = ''
    for _ in range(4):
        ab = ''
        for _ in range(2):
            ab += aa
            aa += z
        z += y
ac_dict = {58: ab, 92: ab, 60: ab, 59: ab, 1: ab, 35: ab}
ad = random.choice(list(ac_dict.values()))
ae_list = [ad for _ in range(6)]
af_list = [ae_list for _ in range(2)]
ag_list = [af_list for _ in range(5)]
ah = random.choice(ag_list)
ai = random.choice(ah)
aj = random.choice(ai)
ak_list = [aj for _ in range(10)]
al = random.choice(ak_list)
if al == '10':
    am = al + ' c1'
elif al == '13':
    am = al + ' c2'
else:
    am = al + ' c3'
def an():
    return am
def ao():
    return an()
ap = ao()
print(ap)