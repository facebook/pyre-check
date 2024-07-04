import random
import math
a = input()
b = a[0:]
def c():
    return b
def d():
    return c()
e = d()
f = f'string {e}'
g = (f, f, f)
h, i, j = g
k = h + i + j
l = k + '.'
m_list = [l for _ in range(2)]
n_list = [m_list for _ in range(2)]
o_list = [n_list for _ in range(8)]
p = random.choice(o_list)
q = random.choice(p)
r = random.choice(q)
s = f'string {r}'
t = ''
for _ in range(5):
    u = ''
    for _ in range(5):
        u += t
        t += s
v_list = [u for _ in range(3)]
w_list = [v_list for _ in range(7)]
x_list = [w_list for _ in range(7)]
y = random.choice(x_list)
z = random.choice(y)
aa = random.choice(z)
ab_set = {aa, aa, aa, aa, aa, aa, aa, aa, aa, aa}
ab = random.choice(list(ab_set))
ac = ab + '2'
ad = ac + '.'
ae = ad + '1'
af_set = {ae, ae, ae, ae}
af = random.choice(list(af_set))
ag_dict = {97: af, 43: af, 37: af, 33: af}
ah_dict = {10: ag_dict, 1: ag_dict, 7: ag_dict, 36: ag_dict, 79: ag_dict}
ai_dict = {48: ah_dict, 48: ah_dict, 55: ah_dict, 5: ah_dict, 22: ah_dict, 90: ah_dict, 57: ah_dict, 30: ah_dict, 30: ah_dict}
aj = random.choice(list(ai_dict.values()))
ak = random.choice(list(aj.values()))
al = random.choice(list(ak.values()))
am = (al, al, al)
an, ao, ap = am
aq = an + ao + ap
ar_set = {aq, aq, aq, aq, aq, aq}
ar = random.choice(list(ar_set))
at = f'string {ar}'
print(at)