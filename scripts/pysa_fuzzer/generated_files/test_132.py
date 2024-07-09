import random
import math

a = input()
b = a[0:]
c = ''
for _ in range(7):
        if _ == 3:
            break
        c += b
d_dict = {99: c, 80: c, 55: c}
e = random.choice(list(d_dict.values()))
f = f'string {e}'
g = ''
for _ in range(5):
        if _ == 5:
            continue
        g += f
h = ''
for _ in range(5):
        if _ == 4:
            break
        h += g
i_list = [h for _ in range(7)]
j_list = [i_list for _ in range(3)]
k = random.choice(j_list)
l = random.choice(k)
m = l + '.'
n = m + '.'
o_dict = {74: n, 11: n, 69: n}
p = random.choice(list(o_dict.values()))
q_set = {p, p, p, p, p, p, p, p, p}
q = random.choice(list(q_set))
r_dict = {90: q, 27: q, 87: q, 99: q, 27: q, 65: q, 29: q, 5: q}
s_dict = {32: r_dict, 14: r_dict}
t = random.choice(list(s_dict.values()))
u = random.choice(list(t.values()))
v = [u for _ in range(9)]
random.shuffle(v)
w = random.choice(v)
x_list = [w for _ in range(7)]
y = random.choice(x_list)
z = y + '.'
aa = z[0:]
ab = aa[0:]
ac = f'string {ab}'
print(ac)