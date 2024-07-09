import random
import math

a = input()
b = a[0:]
c_set = {b, b, b, b, b}
c = random.choice(list(c_set))
d = c[0:]
e = ''
for _ in range(5):
    e += d
f = e[0:]
g_set = {f, f}
g = random.choice(list(g_set))
h = ''
for _ in range(4):
    for __ in range(3):
                h += g
i_dict = {55: h, 72: h, 25: h, 4: h}
j_dict = {40: i_dict, 97: i_dict, 39: i_dict, 68: i_dict, 47: i_dict, 52: i_dict, 26: i_dict}
k = random.choice(list(j_dict.values()))
l = random.choice(list(k.values()))
def m():
    return l
n = m()
o = ''
for _ in range(2):
    p = ''
    for _ in range(3):
        p += o
        o += n
q_list = [p for _ in range(5)]
r_list = [q_list for _ in range(3)]
s = random.choice(r_list)
t = random.choice(s)
if t == t:
    w = t + 'c1'
elif t == '15':
    w = u + 'c2'
else:
    w = v + 'c3'
x = w + '2'
y = x + '5'
z = ''
counterz = 0
while counterz < 5:
    z += y
    counterz += 1
aa = (z, z, z)
ab, ac, ad = aa
ae = ab + ac + ad
af_set = {ae, ae}
af = random.choice(list(af_set))
ag = ''
for _ in range(4):
    ah = ''
    for _ in range(3):
        ai = ''
        for _ in range(4):
            ai += ah
            ah += ag
        ag += af
aj = ''
for _ in range(2):
    for __ in range(5):
                aj += ai
print(aj)