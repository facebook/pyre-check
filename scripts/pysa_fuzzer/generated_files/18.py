import random
import math
a = input()
b_dict = {84: a, 36: a, 66: a, 19: a, 45: a, 91: a, 5: a, 96: a}
c = random.choice(list(b_dict.values()))
d = ''
for _ in range(6):
        if _ == 1:
            break
        d += c
e = ''
for _ in range(8):
        if _ == 3:
            break
        e += d
f_set = {e, e, e, e, e}
f = random.choice(list(f_set))
g_dict = {38: f, 5: f, 42: f, 97: f}
h_dict = {93: g_dict, 34: g_dict, 58: g_dict, 55: g_dict, 9: g_dict, 10: g_dict, 33: g_dict, 21: g_dict}
i_dict = {72: h_dict, 19: h_dict, 93: h_dict, 24: h_dict, 64: h_dict, 54: h_dict, 74: h_dict, 82: h_dict, 89: h_dict, 75: h_dict}
j = random.choice(list(i_dict.values()))
k = random.choice(list(j.values()))
l = random.choice(list(k.values()))
m_list = [l for _ in range(2)]
n_list = [m_list for _ in range(9)]
o = random.choice(n_list)
p = random.choice(o)
q = ''
for _ in range(6):
        if _ == 2:
            break
        q += p
r = f'string {q}'
s_dict = {44: r, 79: r, 32: r, 74: r, 42: r, 96: r}
t_dict = {23: s_dict, 42: s_dict, 57: s_dict}
u = random.choice(list(t_dict.values()))
v = random.choice(list(u.values()))
w = v[0:]
x = w + '.'
y_dict = {93: x, 95: x, 73: x}
z_dict = {73: y_dict, 11: y_dict}
aa = random.choice(list(z_dict.values()))
ab = random.choice(list(aa.values()))
if ab == '3':
    ac = ab + ' c1'
elif ab == '19':
    ac = ab + ' c2'
else:
    ac = ab + ' c3'
ad = ac + '6'
ae = ad + '4'
af = ae + '.'
ag = ''
for _ in range(5):
    for __ in range(4):
                ag += af
def ah():
    return ag
ai = ah()
aj = [ai for _ in range(5)]
random.shuffle(aj)
ak = random.choice(aj)
print(ak)