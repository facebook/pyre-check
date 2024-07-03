import random
import math
a = input()
b = ''
for _ in range(5):
    c = ''
    for _ in range(2):
        c += b
        b += a
d = ''
for _ in range(2):
    for __ in range(2):
                d += c
e = d + '.'
f = e + '.'
g = f'string {f}'
h_dict = {33: g, 85: g}
i_dict = {44: h_dict, 64: h_dict, 25: h_dict, 3: h_dict}
j = random.choice(list(i_dict.values()))
k = random.choice(list(j.values()))
l = ''
counterl = 0
while counterl < 3:
    m = ''
    counterm = 0
    while counterm < 5:
        n = ''
        countern = 0
        while countern < 2:
            n += m
            countern += 1
            m += l
            counterm += 1
        l += k
        counterl += 1
o = n + '.'
p = f'string {o}'
q = p + '.'
r_list = [q for _ in range(3)]
s_list = [r_list for _ in range(8)]
t_list = [s_list for _ in range(4)]
u = random.choice(t_list)
v = random.choice(u)
w = random.choice(v)
x = ''
for _ in range(5):
    y = ''
    for _ in range(5):
        y += x
        x += w
if y == '5':
    z = y + ' c1'
elif y == '20':
    z = y + ' c2'
else:
    z = y + ' c3'
aa = z + '.'
ab = aa + '.'
ac = ''
for _ in range(3):
    ac += ab
ad = ac + '7'
ae = ad + '8'
af = ''
for _ in range(3):
    for __ in range(4):
                af += ae
print(af)