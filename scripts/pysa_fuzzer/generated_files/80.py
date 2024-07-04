import random
import math
a = input()
b = ''
for _ in range(5):
    c = ''
    for _ in range(4):
        c += b
        b += a
d = ''
for _ in range(3):
    for __ in range(2):
                d += c
e = f'string {d}'
f_list = [e for _ in range(7)]
g_list = [f_list for _ in range(6)]
h = random.choice(g_list)
i = random.choice(h)
j_dict = {14: i, 69: i, 92: i, 45: i, 44: i, 60: i}
k_dict = {83: j_dict, 53: j_dict, 41: j_dict, 72: j_dict, 11: j_dict}
l = random.choice(list(k_dict.values()))
m = random.choice(list(l.values()))
n = ''
for _ in range(2):
    for __ in range(3):
                n += m
o = ''
for _ in range(3):
    p = ''
    for _ in range(4):
        q = ''
        for _ in range(3):
            q += p
            p += o
        o += n
def r():
    return q
s = r()
t_list = [s for _ in range(5)]
u_list = [t_list for _ in range(8)]
v_list = [u_list for _ in range(7)]
w = random.choice(v_list)
x = random.choice(w)
y = random.choice(x)
z = ''
for _ in range(4):
    for __ in range(5):
                z += y
if z == '1':
    aa = z + ' c1'
elif z == '11':
    aa = z + ' c2'
else:
    aa = z + ' c3'
ab = aa + '.'
ac = f'string {ab}'
ad = f'string {ac}'
if ad == '1':
    ae = ad + ' c1'
elif ad == '17':
    ae = ad + ' c2'
else:
    ae = ad + ' c3'
af = ''
for _ in range(3):
    for __ in range(4):
                af += ae
ag_list = [af for _ in range(5)]
ah = random.choice(ag_list)
ai = ah[0:]
print(ai)