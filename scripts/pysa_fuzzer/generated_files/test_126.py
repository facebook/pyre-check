import random
import math

a = input()
b = ''
for _ in range(5):
        if _ == 2:
            break
        b += a
c_dict = {37: b, 74: b, 4: b, 17: b, 62: b, 40: b}
d_dict = {8: c_dict, 96: c_dict, 77: c_dict, 99: c_dict}
e_dict = {72: d_dict, 13: d_dict, 65: d_dict, 92: d_dict}
f = random.choice(list(e_dict.values()))
g = random.choice(list(f.values()))
h = random.choice(list(g.values()))
i = ''
for _ in range(7):
        if _ == 5:
            break
        i += h
j = i + '7'
k = j + '.'
l = k[0:]
m = [l for _ in range(6)]
random.shuffle(m)
n = random.choice(m)
o = n[0:]
p = ''
for _ in range(7):
        if _ == 2:
            continue
        p += o
q = p[0:]
r = ''
for _ in range(4):
    s = ''
    for _ in range(5):
        s += r
        r += q
t = s + '6'
u = t + '6'
v = u[0:]
w = v[0:]
x = w + '.'
y = ''
for _ in range(3):
    for __ in range(2):
                y += x
z = [y for _ in range(9)]
random.shuffle(z)
aa = random.choice(z)
ab = aa[0:]
print(ab)