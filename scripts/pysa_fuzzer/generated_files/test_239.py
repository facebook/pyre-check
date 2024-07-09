import random
import math

a = input()
b = a[0:]
c = b[0:]
d = ''
counterd = 0
while counterd < 2:
    d += c
    counterd += 1
e = ''
for _ in range(2):
    for __ in range(4):
                e += d
f = [e for _ in range(9)]
random.shuffle(f)
g = random.choice(f)
h = g[0:]
i = f'string {h}'
j_dict = {54: i, 60: i, 97: i, 11: i, 65: i, 71: i, 39: i, 20: i, 18: i, 86: i}
k_dict = {84: j_dict, 15: j_dict, 87: j_dict, 89: j_dict, 62: j_dict, 47: j_dict, 18: j_dict, 37: j_dict}
l_dict = {54: k_dict, 49: k_dict, 64: k_dict, 34: k_dict, 24: k_dict, 81: k_dict, 46: k_dict, 26: k_dict, 39: k_dict, 2: k_dict}
m = random.choice(list(l_dict.values()))
n = random.choice(list(m.values()))
o = random.choice(list(n.values()))
p_set = {o, o, o, o, o, o, o, o, o, o}
p = random.choice(list(p_set))
q = p + '.'
r = [q for _ in range(5)]
random.shuffle(r)
s = random.choice(r)
t = ''
for _ in range(8):
        if _ == 2:
            continue
        t += s
u = ''
for _ in range(4):
    for __ in range(5):
                u += t
v = ''
for _ in range(4):
    w = ''
    for _ in range(5):
        w += v
        v += u
x = ''
for _ in range(2):
    for __ in range(4):
                x += w
y_set = {x, x, x, x}
y = random.choice(list(y_set))
z = ''
for _ in range(2):
    z += y
aa = (z, z, z)
ab, ac, ad = aa
ae = ab + ac + ad
print(ae)