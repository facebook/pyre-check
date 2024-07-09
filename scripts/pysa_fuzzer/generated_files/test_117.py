import random
import math

a = input()
b = ''
counterb = 0
while counterb < 2:
    b += a
    counterb += 1
c = [b for _ in range(9)]
random.shuffle(c)
d = random.choice(c)
if d == d:
    g = d + 'c1'
elif d == '13':
    g = e + 'c2'
else:
    g = f + 'c3'
h = g[0:]
i = [h for _ in range(9)]
random.shuffle(i)
j = random.choice(i)
k = ''
for _ in range(3):
    k += j
l_set = {k, k, k, k}
l = random.choice(list(l_set))
m = f'string {l}'
n = ''
for _ in range(3):
    n += m
o_dict = {35: n, 43: n, 15: n, 44: n, 86: n, 98: n, 97: n, 66: n}
p_dict = {54: o_dict, 74: o_dict, 73: o_dict}
q_dict = {89: p_dict, 98: p_dict, 20: p_dict}
r = random.choice(list(q_dict.values()))
s = random.choice(list(r.values()))
t = random.choice(list(s.values()))
u_list = [t for _ in range(7)]
v_list = [u_list for _ in range(5)]
w_list = [v_list for _ in range(8)]
x = random.choice(w_list)
y = random.choice(x)
z = random.choice(y)
aa_list = [z for _ in range(9)]
ab = random.choice(aa_list)
if ab == ab:
    ae = ab + 'c1'
elif ab == '19':
    ae = ac + 'c2'
else:
    ae = ad + 'c3'
af = ''
for _ in range(6):
        if _ == 2:
            break
        af += ae
ag_dict = {55: af, 91: af, 60: af, 81: af}
ah = random.choice(list(ag_dict.values()))
ai = ''
for _ in range(3):
    for __ in range(5):
                ai += ah
aj_set = {ai, ai, ai, ai, ai, ai, ai, ai}
aj = random.choice(list(aj_set))
ak = (aj, aj, aj)
al, am, an = ak
ao = al + am + an
print(ao)