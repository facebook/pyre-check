import random
import math

a = input()
b = ''
counterb = 0
while counterb < 3:
    b += a
    counterb += 1
c = b + '7'
d = c + '1'
e_dict = {81: d, 83: d, 3: d, 21: d, 51: d, 74: d, 41: d, 67: d}
f_dict = {53: e_dict, 69: e_dict, 3: e_dict, 67: e_dict, 25: e_dict}
g_dict = {51: f_dict, 82: f_dict, 52: f_dict}
h = random.choice(list(g_dict.values()))
i = random.choice(list(h.values()))
j = random.choice(list(i.values()))
k = ''
for _ in range(5):
    l = ''
    for _ in range(3):
        m = ''
        for _ in range(4):
            m += l
            l += k
        k += j
n_list = [m for _ in range(2)]
o_list = [n_list for _ in range(5)]
p_list = [o_list for _ in range(4)]
q = random.choice(p_list)
r = random.choice(q)
s = random.choice(r)
t = (s, s, s)
u, v, w = t
x = u + v + w
if x == x:
    aa = x + 'c1'
elif x == '14':
    aa = y + 'c2'
else:
    aa = z + 'c3'
ab_set = {aa, aa, aa, aa, aa, aa, aa}
ab = random.choice(list(ab_set))
ac = (ab, ab, ab)
ad, ae, af = ac
ag = ad + ae + af
def ah():
    return ag
def ai():
    return ah()
aj = ai()
ak = ''
for _ in range(4):
    al = ''
    for _ in range(3):
        al += ak
        ak += aj
am_dict = {65: al, 58: al, 82: al, 6: al, 35: al, 14: al, 21: al}
an_dict = {88: am_dict, 7: am_dict, 63: am_dict, 63: am_dict}
ao = random.choice(list(an_dict.values()))
ap = random.choice(list(ao.values()))
if ap == ap:
    at = ap + 'c1'
elif ap == '17':
    at = aq + 'c2'
else:
    at = ar + 'c3'
au_dict = {40: at, 100: at}
av = random.choice(list(au_dict.values()))
if av == av:
    ay = av + 'c1'
elif av == '12':
    ay = aw + 'c2'
else:
    ay = ax + 'c3'
az = ay + '9'
ba = az + '9'
bb_list = [ba for _ in range(9)]
bc_list = [bb_list for _ in range(7)]
bd_list = [bc_list for _ in range(8)]
be = random.choice(bd_list)
bf = random.choice(be)
bg = random.choice(bf)
bh = bg + '6'
bi = bh + '4'
print(bi)