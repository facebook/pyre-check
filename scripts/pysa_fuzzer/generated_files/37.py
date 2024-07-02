import random
import math
a = input()
b = f'string {a}'
if b == '10':
    c = b + ' c1'
elif b == '18':
    c = b + ' c2'
else:
    c = b + ' c3'
d = f'string {c}'
e_set = {d, d, d, d, d, d, d}
e = random.choice(list(e_set))
f = ''
for _ in range(5):
    f += e
g = f + '.'
h_set = {g, g, g, g, g}
h = random.choice(list(h_set))
i = f'string {h}'
j = f'string {i}'
k_list = [j for _ in range(7)]
l_list = [k_list for _ in range(3)]
m_list = [l_list for _ in range(4)]
n = random.choice(m_list)
o = random.choice(n)
p = random.choice(o)
q_dict = {53: p, 5: p, 66: p, 88: p, 69: p}
r_dict = {92: q_dict, 38: q_dict, 24: q_dict, 69: q_dict, 2: q_dict, 29: q_dict, 5: q_dict}
s = random.choice(list(r_dict.values()))
t = random.choice(list(s.values()))
if t == '3':
    u = t + ' c1'
elif t == '15':
    u = t + ' c2'
else:
    u = t + ' c3'
v = u[0:]
w = (v, v, v)
x, y, z = w
aa = x + y + z
ab = (aa, aa, aa)
ac, ad, ae = ab
af = ac + ad + ae
ag = af + '6'
ah = ag + '4'
ai_list = [ah for _ in range(8)]
aj_list = [ai_list for _ in range(9)]
ak = random.choice(aj_list)
al = random.choice(ak)
am = ''
for _ in range(2):
    for __ in range(2):
                am += al
print(am)