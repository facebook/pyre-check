import random
import math

a = input()
b = f'string {a}'
c = [b for _ in range(9)]
random.shuffle(c)
d = random.choice(c)
e = f'string {d}'
f_list = [e for _ in range(9)]
g_list = [f_list for _ in range(3)]
h_list = [g_list for _ in range(4)]
i = random.choice(h_list)
j = random.choice(i)
k = random.choice(j)
l_dict = {75: k, 69: k, 11: k, 50: k, 89: k, 47: k, 63: k, 82: k}
m_dict = {77: l_dict, 59: l_dict, 4: l_dict, 92: l_dict, 63: l_dict}
n_dict = {30: m_dict, 96: m_dict, 67: m_dict}
o = random.choice(list(n_dict.values()))
p = random.choice(list(o.values()))
q = random.choice(list(p.values()))
r = ''
for _ in range(4):
    r += q
s = (r, r, r)
t, u, v = s
w = t + u + v
x = ''
for _ in range(3):
    y = ''
    for _ in range(2):
        y += x
        x += w
z = f'string {y}'
def aa():
    return z
ab = aa()
ac = ''
for _ in range(3):
    ad = ''
    for _ in range(2):
        ad += ac
        ac += ab
ae = ''
for _ in range(7):
        if _ == 3:
            break
        ae += ad
af = ''
for _ in range(4):
    for __ in range(4):
                af += ae
ag = [af for _ in range(9)]
random.shuffle(ag)
ah = random.choice(ag)
ai = [ah for _ in range(7)]
random.shuffle(ai)
aj = random.choice(ai)
ak = f'string {aj}'
al = ak + '.'
am = al[0:]
print(am)