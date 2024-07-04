import random
import math
a = input()
b_list = [a for _ in range(3)]
c_list = [b_list for _ in range(9)]
d = random.choice(c_list)
e = random.choice(d)
f = e + '6'
g = f + '6'
h = ''
for _ in range(4):
    for __ in range(3):
                h += g
i = h[0:]
j = i[0:]
k = (j, j, j)
l, m, n = k
o = l + m + n
if o == '1':
    p = o + ' c1'
elif o == '12':
    p = o + ' c2'
else:
    p = o + ' c3'
q_dict = {57: p, 38: p, 7: p, 16: p, 39: p, 15: p, 3: p}
r_dict = {10: q_dict, 73: q_dict, 72: q_dict, 10: q_dict}
s_dict = {37: r_dict, 88: r_dict}
t = random.choice(list(s_dict.values()))
u = random.choice(list(t.values()))
v = random.choice(list(u.values()))
w_list = [v for _ in range(3)]
x_list = [w_list for _ in range(6)]
y_list = [x_list for _ in range(9)]
z = random.choice(y_list)
aa = random.choice(z)
ab = random.choice(aa)
ac = f'string {ab}'
ad = (ac, ac, ac)
ae, af, ag = ad
ah = ae + af + ag
ai = f'string {ah}'
aj = ''
for _ in range(3):
    ak = ''
    for _ in range(5):
        al = ''
        for _ in range(3):
            al += ak
            ak += aj
        aj += ai
am_dict = {38: al, 99: al, 72: al, 1: al, 60: al, 69: al, 56: al, 56: al, 68: al, 100: al}
an = random.choice(list(am_dict.values()))
ao = an[0:]
def ap():
    return ao
def aq():
    return ap()
ar = aq()
at = ''
for _ in range(2):
    for __ in range(3):
                at += ar
au = f'string {at}'
print(au)