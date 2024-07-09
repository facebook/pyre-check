import random
import math

a = input()
b = a + '.'
c_set = {b, b, b, b, b}
c = random.choice(list(c_set))
d = c[0:]
e_set = {d, d, d, d, d, d, d}
e = random.choice(list(e_set))
f = e[0:]
g = ''
for _ in range(3):
    for __ in range(4):
                g += f
h = (g, g, g)
i, j, k = h
l = i + j + k
m = ''
for _ in range(10):
        if _ == 2:
            break
        m += l
def n():
    return m
o = n()
p_dict = {60: o, 100: o, 57: o, 93: o, 26: o, 25: o, 41: o, 1: o, 90: o, 14: o}
q_dict = {46: p_dict, 75: p_dict, 55: p_dict, 9: p_dict, 19: p_dict, 7: p_dict, 8: p_dict, 74: p_dict}
r_dict = {51: q_dict, 98: q_dict}
s = random.choice(list(r_dict.values()))
t = random.choice(list(s.values()))
u = random.choice(list(t.values()))
v = u + '4'
w = v + '6'
x = ''
counterx = 0
while counterx < 2:
    y = ''
    countery = 0
    while countery < 2:
        y += x
        countery += 1
        x += w
        counterx += 1
def z():
    return y
def aa():
    return z()
ab = aa()
ac = [ab for _ in range(6)]
random.shuffle(ac)
ad = random.choice(ac)
ae = ''
for _ in range(7):
        if _ == 2:
            break
        ae += ad
af = ae[0:]
ag = (af, af, af)
ah, ai, aj = ag
ak = ah + ai + aj
al = ''
for _ in range(7):
        if _ == 5:
            continue
        al += ak
print(al)