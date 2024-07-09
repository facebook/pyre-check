import random
import math

a = input()
b_dict = {13: a, 40: a, 98: a, 87: a, 28: a, 21: a, 39: a}
c = random.choice(list(b_dict.values()))
d = ''
for _ in range(2):
    d += c
e = d + '.'
f_dict = {66: e, 89: e, 63: e, 12: e, 50: e, 3: e, 6: e, 91: e, 62: e, 22: e}
g_dict = {10: f_dict, 94: f_dict, 86: f_dict}
h_dict = {62: g_dict, 83: g_dict, 9: g_dict, 27: g_dict, 95: g_dict, 100: g_dict, 59: g_dict}
i = random.choice(list(h_dict.values()))
j = random.choice(list(i.values()))
k = random.choice(list(j.values()))
l_list = [k for _ in range(7)]
m_list = [l_list for _ in range(9)]
n_list = [m_list for _ in range(9)]
o = random.choice(n_list)
p = random.choice(o)
q = random.choice(p)
r = f'string {q}'
s = ''
for _ in range(4):
    s += r
t = s[0:]
u_list = [t for _ in range(7)]
v_list = [u_list for _ in range(9)]
w = random.choice(v_list)
x = random.choice(w)
y = [x for _ in range(8)]
random.shuffle(y)
z = random.choice(y)
aa = z[0:]
def ab():
    return aa
def ac():
    return ab()
ad = ac()
ae_list = [ad for _ in range(2)]
af_list = [ae_list for _ in range(8)]
ag_list = [af_list for _ in range(8)]
ah = random.choice(ag_list)
ai = random.choice(ah)
aj = random.choice(ai)
ak = [aj for _ in range(10)]
random.shuffle(ak)
al = random.choice(ak)
am = al + '.'
an_list = [am for _ in range(6)]
ao_list = [an_list for _ in range(8)]
ap_list = [ao_list for _ in range(3)]
aq = random.choice(ap_list)
ar = random.choice(aq)
at = random.choice(ar)
if at == at:
    aw = at + 'c1'
elif at == '15':
    aw = au + 'c2'
else:
    aw = av + 'c3'
ax = aw + '.'
print(ax)