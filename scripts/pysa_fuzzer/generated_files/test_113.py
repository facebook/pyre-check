import random
import math

a = input()
b = a[0:]
c = b[0:]
d = ''
for _ in range(4):
    d += c
e = f'string {d}'
f = ''
for _ in range(5):
    f += e
g = ''
for _ in range(2):
    for __ in range(2):
                g += f
h_set = {g, g, g}
h = random.choice(list(h_set))
i = (h, h, h)
j, k, l = i
m = j + k + l
def n():
    return m
def o():
    return n()
p = o()
q = p + '8'
r = q[0:]
s = f'string {r}'
t_list = [s for _ in range(6)]
u_list = [t_list for _ in range(9)]
v = random.choice(u_list)
w = random.choice(v)
x_dict = {22: w, 41: w, 100: w, 70: w}
y_dict = {97: x_dict, 96: x_dict}
z_dict = {51: y_dict, 53: y_dict}
aa = random.choice(list(z_dict.values()))
ab = random.choice(list(aa.values()))
ac = random.choice(list(ab.values()))
ad = ''
for _ in range(6):
        if _ == 3:
            break
        ad += ac
ae = [ad for _ in range(8)]
random.shuffle(ae)
af = random.choice(ae)
ag = ''
for _ in range(2):
    ah = ''
    for _ in range(5):
        ai = ''
        for _ in range(4):
            ai += ah
            ah += ag
        ag += af
aj = ai[0:]
print(aj)