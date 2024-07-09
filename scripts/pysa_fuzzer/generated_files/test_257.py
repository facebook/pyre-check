import random
import math

a = input()
b_list = [a for _ in range(6)]
c_list = [b_list for _ in range(5)]
d_list = [c_list for _ in range(10)]
e = random.choice(d_list)
f = random.choice(e)
g = random.choice(f)
h_dict = {79: g, 68: g, 70: g, 75: g}
i_dict = {73: h_dict, 11: h_dict, 13: h_dict, 42: h_dict, 82: h_dict}
j_dict = {87: i_dict, 31: i_dict, 85: i_dict, 22: i_dict, 65: i_dict, 72: i_dict, 18: i_dict, 29: i_dict, 15: i_dict, 2: i_dict}
k = random.choice(list(j_dict.values()))
l = random.choice(list(k.values()))
m = random.choice(list(l.values()))
n_dict = {88: m, 59: m}
o_dict = {22: n_dict, 85: n_dict, 68: n_dict, 30: n_dict, 6: n_dict, 83: n_dict, 15: n_dict, 81: n_dict, 72: n_dict}
p_dict = {40: o_dict, 96: o_dict, 94: o_dict, 23: o_dict, 57: o_dict, 50: o_dict}
q = random.choice(list(p_dict.values()))
r = random.choice(list(q.values()))
s = random.choice(list(r.values()))
t = s + '.'
def u():
    return t
v = u()
w = ''
for _ in range(5):
        if _ == 2:
            continue
        w += v
def x():
    return w
y = x()
z = ''
for _ in range(5):
        if _ == 4:
            break
        z += y
aa_dict = {25: z, 38: z, 89: z, 76: z, 62: z, 11: z, 15: z, 74: z, 48: z}
ab = random.choice(list(aa_dict.values()))
ac = [ab for _ in range(7)]
random.shuffle(ac)
ad = random.choice(ac)
ae_list = [ad for _ in range(6)]
af = random.choice(ae_list)
ag = [af for _ in range(5)]
random.shuffle(ag)
ah = random.choice(ag)
ai = [ah for _ in range(7)]
random.shuffle(ai)
aj = random.choice(ai)
ak = ''
for _ in range(4):
    ak += aj
al = ''
for _ in range(5):
        if _ == 2:
            break
        al += ak
am = ''
for _ in range(7):
        if _ == 3:
            continue
        am += al
an = ''
for _ in range(10):
        if _ == 4:
            continue
        an += am
ao = an + '.'
print(ao)