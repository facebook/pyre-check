import random
import math
a = input()
def b():
    return a
def c():
    return b()
d = c()
e = d + '.'
f = f'string {e}'
g_dict = {58: f, 24: f, 5: f, 65: f, 22: f}
h_dict = {78: g_dict, 15: g_dict, 39: g_dict, 56: g_dict}
i_dict = {66: h_dict, 52: h_dict, 52: h_dict, 85: h_dict, 99: h_dict, 38: h_dict, 26: h_dict}
j = random.choice(list(i_dict.values()))
k = random.choice(list(j.values()))
l = random.choice(list(k.values()))
m_list = [l for _ in range(2)]
n_list = [m_list for _ in range(8)]
o_list = [n_list for _ in range(5)]
p = random.choice(o_list)
q = random.choice(p)
r = random.choice(q)
def s():
    return r
t = s()
u = ''
for _ in range(4):
    u += t
v_set = {u, u, u}
v = random.choice(list(v_set))
w_dict = {22: v, 33: v, 43: v, 9: v, 79: v}
x = random.choice(list(w_dict.values()))
y = x + '5'
z = y + '7'
aa = z + '6'
ab_dict = {95: aa, 84: aa, 73: aa}
ac_dict = {72: ab_dict, 46: ab_dict, 1: ab_dict, 49: ab_dict, 52: ab_dict, 71: ab_dict, 1: ab_dict}
ad_dict = {53: ac_dict, 44: ac_dict, 26: ac_dict, 92: ac_dict, 6: ac_dict, 17: ac_dict, 72: ac_dict, 52: ac_dict, 20: ac_dict, 10: ac_dict}
ae = random.choice(list(ad_dict.values()))
af = random.choice(list(ae.values()))
ag = random.choice(list(af.values()))
if ag == '8':
    ah = ag + ' c1'
elif ag == '18':
    ah = ag + ' c2'
else:
    ah = ag + ' c3'
ai_set = {ah, ah, ah, ah}
ai = random.choice(list(ai_set))
aj = (ai, ai, ai)
ak, al, am = aj
an = ak + al + am
ao = ''
for _ in range(9):
        if _ == 3:
            continue
        ao += an
ap = ao + '.'
aq = f'string {ap}'
ar = aq + '3'
at = ar + '9'
print(at)