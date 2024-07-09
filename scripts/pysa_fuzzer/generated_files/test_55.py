import random
import math

a = input()
b = ''
for _ in range(5):
        if _ == 4:
            continue
        b += a
c_dict = {71: b, 78: b, 80: b, 94: b, 25: b, 33: b}
d_dict = {21: c_dict, 97: c_dict, 10: c_dict, 41: c_dict}
e_dict = {4: d_dict, 88: d_dict, 59: d_dict, 67: d_dict}
f = random.choice(list(e_dict.values()))
g = random.choice(list(f.values()))
h = random.choice(list(g.values()))
i_dict = {32: h, 85: h, 47: h, 60: h, 31: h, 98: h, 21: h, 29: h}
j_dict = {99: i_dict, 63: i_dict, 4: i_dict, 76: i_dict, 8: i_dict, 16: i_dict, 13: i_dict}
k = random.choice(list(j_dict.values()))
l = random.choice(list(k.values()))
m = ''
for _ in range(3):
    n = ''
    for _ in range(2):
        n += m
        m += l
o = n + '6'
p = o + '5'
q_dict = {53: p, 10: p, 41: p, 56: p}
r_dict = {23: q_dict, 66: q_dict, 88: q_dict}
s = random.choice(list(r_dict.values()))
t = random.choice(list(s.values()))
u = ''
for _ in range(5):
        if _ == 4:
            break
        u += t
v = u + '.'
w_list = [v for _ in range(2)]
x_list = [w_list for _ in range(8)]
y = random.choice(x_list)
z = random.choice(y)
aa = ''
for _ in range(3):
    ab = ''
    for _ in range(2):
        ab += aa
        aa += z
ac = ''
for _ in range(4):
    for __ in range(5):
                ac += ab
ad = ac + '3'
ae = ad + '9'
af = ae + '9'
ag = [af for _ in range(8)]
random.shuffle(ag)
ah = random.choice(ag)
ai = ''
for _ in range(10):
        if _ == 5:
            break
        ai += ah
def aj():
    return ai
ak = aj()
def al():
    return ak
def am():
    return al()
def an():
    return am()
ao = an()
ap_list = [ao for _ in range(10)]
aq = random.choice(ap_list)
ar = aq[0:]
print(ar)