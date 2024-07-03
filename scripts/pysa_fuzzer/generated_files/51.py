import random
import math
a = input()
b = [a for _ in range(10)]
random.shuffle(b)
c = random.choice(b)
d = f'string {c}'
e_list = [d for _ in range(3)]
f_list = [e_list for _ in range(7)]
g = random.choice(f_list)
h = random.choice(g)
i_dict = {68: h, 70: h, 100: h, 15: h}
j_dict = {36: i_dict, 46: i_dict, 16: i_dict, 49: i_dict, 98: i_dict, 23: i_dict, 24: i_dict, 99: i_dict, 2: i_dict}
k = random.choice(list(j_dict.values()))
l = random.choice(list(k.values()))
m = ''
for _ in range(8):
        if _ == 5:
            continue
        m += l
n = m[0:]
o = [n for _ in range(6)]
random.shuffle(o)
p = random.choice(o)
q = [p for _ in range(9)]
random.shuffle(q)
r = random.choice(q)
s_list = [r for _ in range(7)]
t_list = [s_list for _ in range(7)]
u = random.choice(t_list)
v = random.choice(u)
w = ''
for _ in range(2):
    x = ''
    for _ in range(3):
        y = ''
        for _ in range(2):
            y += x
            x += w
        w += v
z = ''
for _ in range(10):
        if _ == 5:
            continue
        z += y
aa = (z, z, z)
ab, ac, ad = aa
ae = ab + ac + ad
af = [ae for _ in range(7)]
random.shuffle(af)
ag = random.choice(af)
ah = ''
for _ in range(3):
    ai = ''
    for _ in range(2):
        ai += ah
        ah += ag
aj = ai + '3'
ak = aj + '8'
al = ak + '1'
am = al + '.'
an_dict = {86: am, 13: am, 13: am, 11: am, 36: am}
ao_dict = {9: an_dict, 24: an_dict, 76: an_dict, 14: an_dict, 24: an_dict, 5: an_dict}
ap_dict = {12: ao_dict, 61: ao_dict, 30: ao_dict}
aq = random.choice(list(ap_dict.values()))
ar = random.choice(list(aq.values()))
at = random.choice(list(ar.values()))
au_list = [at for _ in range(7)]
av_list = [au_list for _ in range(7)]
aw = random.choice(av_list)
ax = random.choice(aw)
print(ax)