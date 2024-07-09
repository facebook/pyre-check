import random
import math

a = input()
b = a + '3'
c = b + '.'
d = c + '.'
e = (d, d, d)
f, g, h = e
i = f + g + h
j_set = {i, i, i}
j = random.choice(list(j_set))
k = j + '8'
l = k + '1'
m_dict = {42: l, 9: l}
n_dict = {66: m_dict, 71: m_dict, 75: m_dict, 59: m_dict, 56: m_dict}
o_dict = {19: n_dict, 14: n_dict, 20: n_dict, 26: n_dict, 68: n_dict}
p = random.choice(list(o_dict.values()))
q = random.choice(list(p.values()))
r = random.choice(list(q.values()))
s_list = [r for _ in range(6)]
t_list = [s_list for _ in range(2)]
u = random.choice(t_list)
v = random.choice(u)
w_list = [v for _ in range(8)]
x_list = [w_list for _ in range(10)]
y_list = [x_list for _ in range(2)]
z = random.choice(y_list)
aa = random.choice(z)
ab = random.choice(aa)
ac = ab[0:]
ad = ''
counterad = 0
while counterad < 4:
    ae = ''
    counterae = 0
    while counterae < 2:
        ae += ad
        counterae += 1
        ad += ac
        counterad += 1
af_list = [ae for _ in range(10)]
ag_list = [af_list for _ in range(10)]
ah_list = [ag_list for _ in range(9)]
ai = random.choice(ah_list)
aj = random.choice(ai)
ak = random.choice(aj)
al_list = [ak for _ in range(5)]
am = random.choice(al_list)
an = ''
for _ in range(3):
    for __ in range(3):
                an += am
ao = an + '.'
ap = (ao, ao, ao)
aq, ar, at = ap
au = aq + ar + at
av = ''
for _ in range(2):
    for __ in range(5):
                av += au
aw = av + '.'
print(aw)