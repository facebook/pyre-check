import random
import math

a = input()
b = ''
counterb = 0
while counterb < 5:
    c = ''
    counterc = 0
    while counterc < 4:
        c += b
        counterc += 1
        b += a
        counterb += 1
d_dict = {81: c, 19: c, 71: c}
e_dict = {58: d_dict, 3: d_dict, 67: d_dict, 71: d_dict, 43: d_dict}
f_dict = {24: e_dict, 99: e_dict, 39: e_dict, 26: e_dict, 56: e_dict, 2: e_dict, 38: e_dict, 75: e_dict, 89: e_dict}
g = random.choice(list(f_dict.values()))
h = random.choice(list(g.values()))
i = random.choice(list(h.values()))
j = i[0:]
k_set = {j, j, j, j, j, j, j, j, j, j}
k = random.choice(list(k_set))
l_dict = {25: k, 88: k}
m_dict = {19: l_dict, 16: l_dict, 22: l_dict, 21: l_dict, 69: l_dict, 31: l_dict}
n_dict = {58: m_dict, 100: m_dict, 25: m_dict, 38: m_dict, 70: m_dict, 70: m_dict}
o = random.choice(list(n_dict.values()))
p = random.choice(list(o.values()))
q = random.choice(list(p.values()))
r = ''
counterr = 0
while counterr < 5:
    s = ''
    counters = 0
    while counters < 5:
        s += r
        counters += 1
        r += q
        counterr += 1
t_list = [s for _ in range(2)]
u_list = [t_list for _ in range(10)]
v_list = [u_list for _ in range(4)]
w = random.choice(v_list)
x = random.choice(w)
y = random.choice(x)
z = ''
for _ in range(6):
        if _ == 5:
            break
        z += y
if z == z:
    ac = z + 'c1'
elif z == '18':
    ac = aa + 'c2'
else:
    ac = ab + 'c3'
ad = f'string {ac}'
ae = ''
for _ in range(4):
    for __ in range(3):
                ae += ad
def af():
    return ae
ag = af()
ah_set = {ag, ag, ag, ag, ag, ag, ag, ag, ag}
ah = random.choice(list(ah_set))
ai = ah[0:]
aj = ai + '.'
ak_list = [aj for _ in range(10)]
al_list = [ak_list for _ in range(10)]
am_list = [al_list for _ in range(10)]
an = random.choice(am_list)
ao = random.choice(an)
ap = random.choice(ao)
aq_list = [ap for _ in range(4)]
ar_list = [aq_list for _ in range(9)]
at = random.choice(ar_list)
au = random.choice(at)
av = f'string {au}'
print(av)