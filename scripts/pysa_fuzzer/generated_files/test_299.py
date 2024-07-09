import random
import math

a = input()
b = a + '.'
c = b + '.'
d = ''
for _ in range(5):
        if _ == 1:
            continue
        d += c
e = ''
for _ in range(4):
    for __ in range(2):
                e += d
f = ''
for _ in range(4):
    g = ''
    for _ in range(3):
        g += f
        f += e
h = ''
for _ in range(10):
        if _ == 1:
            break
        h += g
i_set = {h, h, h, h, h, h, h, h, h}
i = random.choice(list(i_set))
j = ''
for _ in range(10):
        if _ == 2:
            continue
        j += i
k = [j for _ in range(6)]
random.shuffle(k)
l = random.choice(k)
m = ''
for _ in range(2):
    for __ in range(3):
                m += l
def n():
    return m
o = n()
p_dict = {73: o, 32: o, 44: o}
q_dict = {31: p_dict, 23: p_dict, 53: p_dict, 91: p_dict, 97: p_dict, 44: p_dict, 1: p_dict, 100: p_dict}
r = random.choice(list(q_dict.values()))
s = random.choice(list(r.values()))
t_set = {s, s, s, s, s}
t = random.choice(list(t_set))
u = t + '.'
v = ''
for _ in range(2):
    v += u
w = f'string {v}'
x = ''
for _ in range(3):
    for __ in range(4):
                x += w
y = ''
for _ in range(8):
        if _ == 1:
            break
        y += x
print(y)