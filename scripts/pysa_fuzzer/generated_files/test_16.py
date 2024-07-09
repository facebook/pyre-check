import random
import math

a = input()
if a == a:
    d = a + 'c1'
elif a == '11':
    d = b + 'c2'
else:
    d = c + 'c3'
e_set = {d, d, d, d, d, d, d, d}
e = random.choice(list(e_set))
f_set = {e, e, e, e, e}
f = random.choice(list(f_set))
g = ''
for _ in range(8):
        if _ == 2:
            break
        g += f
h = g + '1'
i = h + '3'
j = i + '9'
k = [j for _ in range(7)]
random.shuffle(k)
l = random.choice(k)
m = f'string {l}'
n_list = [m for _ in range(9)]
o_list = [n_list for _ in range(9)]
p_list = [o_list for _ in range(10)]
q = random.choice(p_list)
r = random.choice(q)
s = random.choice(r)
t_dict = {28: s, 10: s, 38: s, 55: s, 43: s, 55: s}
u_dict = {37: t_dict, 44: t_dict}
v = random.choice(list(u_dict.values()))
w = random.choice(list(v.values()))
x = [w for _ in range(5)]
random.shuffle(x)
y = random.choice(x)
z_dict = {31: y, 43: y, 22: y}
aa_dict = {45: z_dict, 47: z_dict, 79: z_dict, 40: z_dict, 46: z_dict, 37: z_dict, 6: z_dict, 53: z_dict, 2: z_dict, 71: z_dict}
ab = random.choice(list(aa_dict.values()))
ac = random.choice(list(ab.values()))
ad = ''
for _ in range(5):
    ae = ''
    for _ in range(5):
        af = ''
        for _ in range(3):
            af += ae
            ae += ad
        ad += ac
ag = af + '.'
ah = (ag, ag, ag)
ai, aj, ak = ah
al = ai + aj + ak
am = f'string {al}'
an = ''
for _ in range(2):
    ao = ''
    for _ in range(4):
        ao += an
        an += am
ap = ao + '6'
aq = ap + '1'
ar = aq + '.'
print(ar)