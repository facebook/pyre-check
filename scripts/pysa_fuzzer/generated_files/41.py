import random
import math
a = input()
b = ''
counterb = 0
while counterb < 2:
    c = ''
    counterc = 0
    while counterc < 4:
        c += b
        counterc += 1
        b += a
        counterb += 1
d_set = {c, c, c, c, c, c, c}
d = random.choice(list(d_set))
e = d + '2'
f = e + '8'
g_list = [f for _ in range(8)]
h_list = [g_list for _ in range(10)]
i = random.choice(h_list)
j = random.choice(i)
k = ''
for _ in range(2):
    for __ in range(2):
                k += j
l = f'string {k}'
m_set = {l, l, l, l, l}
m = random.choice(list(m_set))
n = [m for _ in range(9)]
random.shuffle(n)
o = random.choice(n)
p = (o, o, o)
q, r, s = p
t = q + r + s
u_dict = {50: t, 52: t, 82: t, 42: t, 42: t, 48: t, 47: t, 12: t, 74: t}
v_dict = {99: u_dict, 7: u_dict, 49: u_dict, 53: u_dict, 10: u_dict, 89: u_dict, 73: u_dict, 11: u_dict, 27: u_dict}
w_dict = {36: v_dict, 59: v_dict}
x = random.choice(list(w_dict.values()))
y = random.choice(list(x.values()))
z = random.choice(list(y.values()))
aa = [z for _ in range(7)]
random.shuffle(aa)
ab = random.choice(aa)
ac = ab + '2'
ad = ac + '4'
ae = ad + '3'
af = (ae, ae, ae)
ag, ah, ai = af
aj = ag + ah + ai
ak = f'string {aj}'
al = ak[0:]
def am():
    return al
an = am()
ao = (an, an, an)
ap, aq, ar = ao
at = ap + aq + ar
print(at)