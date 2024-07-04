import random
import math
a = input()
def b():
    return a
def c():
    return b()
d = c()
e = ''
for _ in range(2):
    for __ in range(2):
                e += d
f = e + '.'
g_dict = {33: f, 45: f, 96: f, 47: f, 41: f, 38: f, 17: f, 24: f, 77: f, 75: f}
h_dict = {46: g_dict, 28: g_dict, 15: g_dict, 2: g_dict, 38: g_dict, 22: g_dict}
i_dict = {21: h_dict, 85: h_dict, 59: h_dict, 53: h_dict, 81: h_dict, 86: h_dict}
j = random.choice(list(i_dict.values()))
k = random.choice(list(j.values()))
l = random.choice(list(k.values()))
def m():
    return l
n = m()
o = n + '6'
p = o + '9'
q = p + '8'
r = q[0:]
s = f'string {r}'
t = f'string {s}'
u = t + '.'
v = u + '.'
w = f'string {v}'
x = ''
for _ in range(5):
    x += w
y = ''
for _ in range(9):
        if _ == 2:
            continue
        y += x
z_list = [y for _ in range(4)]
aa_list = [z_list for _ in range(3)]
ab = random.choice(aa_list)
ac = random.choice(ab)
ad_dict = {15: ac, 92: ac, 16: ac, 74: ac, 26: ac, 47: ac, 18: ac, 33: ac}
ae = random.choice(list(ad_dict.values()))
af = ae + '.'
ag = ''
for _ in range(5):
        if _ == 1:
            break
        ag += af
print(ag)