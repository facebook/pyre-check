import random
import math

a = input()
b = ''
for _ in range(7):
        if _ == 2:
            break
        b += a
c_dict = {42: b, 14: b, 32: b, 8: b, 47: b, 76: b, 11: b}
d_dict = {95: c_dict, 82: c_dict, 100: c_dict, 93: c_dict, 17: c_dict, 25: c_dict}
e_dict = {73: d_dict, 89: d_dict, 85: d_dict}
f = random.choice(list(e_dict.values()))
g = random.choice(list(f.values()))
h = random.choice(list(g.values()))
i = (h, h, h)
j, k, l = i
m = j + k + l
n = [m for _ in range(9)]
random.shuffle(n)
o = random.choice(n)
p = ''
for _ in range(5):
    for __ in range(2):
                p += o
q_set = {p, p, p, p, p, p, p}
q = random.choice(list(q_set))
r = f'string {q}'
s = r + '1'
t = s + '6'
u = t + '6'
v = ''
for _ in range(5):
    for __ in range(3):
                v += u
w_dict = {56: v, 11: v, 19: v}
x_dict = {15: w_dict, 23: w_dict, 38: w_dict}
y = random.choice(list(x_dict.values()))
z = random.choice(list(y.values()))
if z == z:
    ac = z + 'c1'
elif z == '12':
    ac = aa + 'c2'
else:
    ac = ab + 'c3'
ad = ''
for _ in range(7):
        if _ == 2:
            break
        ad += ac
ae_list = [ad for _ in range(4)]
af_list = [ae_list for _ in range(7)]
ag_list = [af_list for _ in range(9)]
ah = random.choice(ag_list)
ai = random.choice(ah)
aj = random.choice(ai)
ak = aj + '9'
al = ''
for _ in range(6):
        if _ == 2:
            break
        al += ak
am = al + '.'
an = ''
for _ in range(4):
    an += am
ao_list = [an for _ in range(7)]
ap_list = [ao_list for _ in range(7)]
aq_list = [ap_list for _ in range(10)]
ar = random.choice(aq_list)
at = random.choice(ar)
au = random.choice(at)
print(au)