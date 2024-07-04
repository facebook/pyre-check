import random
import math
a = input()
b = ''
for _ in range(9):
        if _ == 2:
            continue
        b += a
c = [b for _ in range(5)]
random.shuffle(c)
d = random.choice(c)
e = d + '.'
f = ''
for _ in range(3):
    f += e
g = f'string {f}'
h = ''
for _ in range(9):
        if _ == 1:
            continue
        h += g
i_list = [h for _ in range(4)]
j = random.choice(i_list)
k = j + '3'
l = k + '8'
m = l + '.'
n = m[0:]
o = f'string {n}'
p = o + '.'
q_list = [p for _ in range(2)]
r_list = [q_list for _ in range(2)]
s_list = [r_list for _ in range(10)]
t = random.choice(s_list)
u = random.choice(t)
v = random.choice(u)
w = ''
for _ in range(8):
        if _ == 3:
            continue
        w += v
x_set = {w, w, w, w, w, w}
x = random.choice(list(x_set))
y_set = {x, x, x, x, x, x}
y = random.choice(list(y_set))
z = y + '7'
aa = z + '5'
ab = aa + '5'
ac_dict = {21: ab, 15: ab, 52: ab, 66: ab, 54: ab, 46: ab, 24: ab, 7: ab}
ad_dict = {15: ac_dict, 50: ac_dict, 50: ac_dict, 67: ac_dict, 97: ac_dict, 70: ac_dict, 32: ac_dict, 38: ac_dict, 89: ac_dict, 12: ac_dict}
ae = random.choice(list(ad_dict.values()))
af = random.choice(list(ae.values()))
print(af)