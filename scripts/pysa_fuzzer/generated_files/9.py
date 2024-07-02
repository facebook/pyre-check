import random
import math
a = input()
b = f'string {a}'
c_list = [b for _ in range(5)]
d_list = [c_list for _ in range(5)]
e = random.choice(d_list)
f = random.choice(e)
g = (f, f, f)
h, i, j = g
k = h + i + j
l_set = {k, k, k}
l = random.choice(list(l_set))
m_set = {l, l, l, l, l, l, l}
m = random.choice(list(m_set))
n = ''
for _ in range(5):
        if _ == 5:
            break
        n += m
o = n[0:]
p = ''
for _ in range(2):
    q = ''
    for _ in range(3):
        q += p
        p += o
r = (q, q, q)
s, t, u = r
v = s + t + u
w_dict = {1: v, 91: v, 13: v, 81: v, 29: v, 65: v}
x_dict = {64: w_dict, 25: w_dict}
y_dict = {32: x_dict, 25: x_dict}
z = random.choice(list(y_dict.values()))
aa = random.choice(list(z.values()))
ab = random.choice(list(aa.values()))
ac = ''
for _ in range(5):
    ad = ''
    for _ in range(2):
        ad += ac
        ac += ab
ae_list = [ad for _ in range(2)]
af_list = [ae_list for _ in range(8)]
ag = random.choice(af_list)
ah = random.choice(ag)
ai_dict = {51: ah, 46: ah}
aj_dict = {19: ai_dict, 15: ai_dict, 8: ai_dict}
ak_dict = {45: aj_dict, 5: aj_dict}
al = random.choice(list(ak_dict.values()))
am = random.choice(list(al.values()))
an = random.choice(list(am.values()))
def ao():
    return an
ap = ao()
aq = ''
for _ in range(10):
        if _ == 4:
            break
        aq += ap
ar = (aq, aq, aq)
at, au, av = ar
aw = at + au + av
ax = aw + '.'
ay = ''
counteray = 0
while counteray < 4:
    az = ''
    counteraz = 0
    while counteraz < 5:
        az += ay
        counteraz += 1
        ay += ax
        counteray += 1
print(az)