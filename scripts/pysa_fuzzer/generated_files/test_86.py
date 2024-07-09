import random
import math

a = input()
b_set = {a, a, a, a, a}
b = random.choice(list(b_set))
c = b[0:]
d = c + '1'
e = d + '9'
f = e + '1'
g_list = [f for _ in range(10)]
h_list = [g_list for _ in range(9)]
i_list = [h_list for _ in range(7)]
j = random.choice(i_list)
k = random.choice(j)
l = random.choice(k)
m_set = {l, l, l, l, l}
m = random.choice(list(m_set))
n = f'string {m}'
o_set = {n, n, n, n, n, n}
o = random.choice(list(o_set))
p_dict = {87: o, 82: o, 13: o, 30: o, 66: o, 12: o}
q_dict = {86: p_dict, 33: p_dict, 53: p_dict, 91: p_dict, 61: p_dict, 16: p_dict, 52: p_dict, 63: p_dict, 82: p_dict}
r_dict = {47: q_dict, 100: q_dict, 30: q_dict, 84: q_dict, 12: q_dict, 87: q_dict, 17: q_dict, 52: q_dict, 10: q_dict, 14: q_dict}
s = random.choice(list(r_dict.values()))
t = random.choice(list(s.values()))
u = random.choice(list(t.values()))
v = ''
for _ in range(2):
    for __ in range(3):
                v += u
def w():
    return v
def x():
    return w()
y = x()
z = y + '4'
aa_set = {z, z}
aa = random.choice(list(aa_set))
ab = ''
for _ in range(3):
    for __ in range(3):
                ab += aa
ac = f'string {ab}'
ad = [ac for _ in range(10)]
random.shuffle(ad)
ae = random.choice(ad)
af_dict = {89: ae, 51: ae, 70: ae, 72: ae, 1: ae}
ag_dict = {44: af_dict, 21: af_dict, 53: af_dict, 43: af_dict}
ah = random.choice(list(ag_dict.values()))
ai = random.choice(list(ah.values()))
aj = ''
for _ in range(10):
        if _ == 3:
            break
        aj += ai
ak = ''
for _ in range(3):
    ak += aj
print(ak)