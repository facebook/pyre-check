import random
import math

a = input()
b = ''
for _ in range(4):
    c = ''
    for _ in range(3):
        d = ''
        for _ in range(3):
            d += c
            c += b
        b += a
e = d + '9'
f = e + '1'
g = f + '4'
h_list = [g for _ in range(4)]
i_list = [h_list for _ in range(6)]
j_list = [i_list for _ in range(9)]
k = random.choice(j_list)
l = random.choice(k)
m = random.choice(l)
n = ''
for _ in range(4):
    for __ in range(3):
                n += m
o = ''
for _ in range(4):
    for __ in range(5):
                o += n
p_dict = {89: o, 13: o, 39: o}
q_dict = {86: p_dict, 12: p_dict, 12: p_dict, 28: p_dict, 18: p_dict}
r_dict = {73: q_dict, 52: q_dict, 69: q_dict, 47: q_dict, 49: q_dict}
s = random.choice(list(r_dict.values()))
t = random.choice(list(s.values()))
u = random.choice(list(t.values()))
v = ''
for _ in range(7):
        if _ == 5:
            continue
        v += u
w = ''
for _ in range(9):
        if _ == 2:
            break
        w += v
x_set = {w, w, w}
x = random.choice(list(x_set))
y_dict = {51: x, 94: x, 21: x, 63: x, 90: x, 62: x, 41: x}
z_dict = {83: y_dict, 76: y_dict, 55: y_dict, 97: y_dict, 19: y_dict, 20: y_dict, 6: y_dict, 74: y_dict, 68: y_dict}
aa_dict = {66: z_dict, 20: z_dict, 12: z_dict, 87: z_dict, 84: z_dict}
ab = random.choice(list(aa_dict.values()))
ac = random.choice(list(ab.values()))
ad = random.choice(list(ac.values()))
ae_list = [ad for _ in range(3)]
af_list = [ae_list for _ in range(5)]
ag = random.choice(af_list)
ah = random.choice(ag)
ai = ''
for _ in range(4):
    for __ in range(2):
                ai += ah
aj = f'string {ai}'
ak_list = [aj for _ in range(2)]
al = random.choice(ak_list)
am = ''
for _ in range(10):
        if _ == 1:
            continue
        am += al
if am == am:
    ap = am + 'c1'
elif am == '14':
    ap = an + 'c2'
else:
    ap = ao + 'c3'
aq = ap[0:]
ar = f'string {aq}'
print(ar)