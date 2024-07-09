import random
import math

a = input()
b = a[0:]
c_dict = {24: b, 30: b, 93: b, 89: b}
d_dict = {69: c_dict, 27: c_dict, 56: c_dict, 61: c_dict, 76: c_dict, 36: c_dict, 73: c_dict, 14: c_dict, 79: c_dict, 43: c_dict}
e_dict = {49: d_dict, 75: d_dict, 88: d_dict, 67: d_dict, 96: d_dict, 26: d_dict}
f = random.choice(list(e_dict.values()))
g = random.choice(list(f.values()))
h = random.choice(list(g.values()))
i = f'string {h}'
j = ''
for _ in range(7):
        if _ == 1:
            break
        j += i
def k():
    return j
def l():
    return k()
def m():
    return l()
n = m()
o = ''
for _ in range(2):
    p = ''
    for _ in range(5):
        q = ''
        for _ in range(2):
            q += p
            p += o
        o += n
r_list = [q for _ in range(3)]
s_list = [r_list for _ in range(7)]
t = random.choice(s_list)
u = random.choice(t)
v = (u, u, u)
w, x, y = v
z = w + x + y
aa_set = {z, z, z, z, z, z, z, z, z}
aa = random.choice(list(aa_set))
def ab():
    return aa
ac = ab()
ad = ''
for _ in range(5):
    for __ in range(5):
                ad += ac
ae = ''
for _ in range(5):
    for __ in range(2):
                ae += ad
af_set = {ae, ae, ae, ae, ae, ae, ae, ae, ae, ae}
af = random.choice(list(af_set))
ag = ''
for _ in range(6):
        if _ == 3:
            continue
        ag += af
ah = ag[0:]
ai = ''
for _ in range(10):
        if _ == 2:
            break
        ai += ah
aj = f'string {ai}'
ak = ''
for _ in range(4):
    al = ''
    for _ in range(4):
        am = ''
        for _ in range(3):
            am += al
            al += ak
        ak += aj
print(am)