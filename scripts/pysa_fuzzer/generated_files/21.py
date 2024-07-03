import random
import math
a = input()
b = a + '8'
if b == '4':
    c = b + ' c1'
elif b == '13':
    c = b + ' c2'
else:
    c = b + ' c3'
d = ''
for _ in range(4):
    d += c
e_dict = {96: d, 84: d, 43: d, 51: d, 42: d, 24: d}
f_dict = {77: e_dict, 40: e_dict, 89: e_dict, 46: e_dict, 4: e_dict, 80: e_dict, 84: e_dict, 4: e_dict, 60: e_dict}
g = random.choice(list(f_dict.values()))
h = random.choice(list(g.values()))
i = ''
for _ in range(5):
    for __ in range(5):
                i += h
j = (i, i, i)
k, l, m = j
n = k + l + m
o_dict = {21: n, 7: n, 98: n, 60: n, 10: n, 14: n, 92: n, 52: n, 84: n, 92: n}
p_dict = {82: o_dict, 87: o_dict, 50: o_dict}
q = random.choice(list(p_dict.values()))
r = random.choice(list(q.values()))
s = ''
for _ in range(2):
    for __ in range(4):
                s += r
t_set = {s, s, s, s, s, s, s, s, s, s}
t = random.choice(list(t_set))
u = ''
for _ in range(4):
    for __ in range(3):
                u += t
if u == '8':
    v = u + ' c1'
elif u == '20':
    v = u + ' c2'
else:
    v = u + ' c3'
w = v + '6'
x = [w for _ in range(8)]
random.shuffle(x)
y = random.choice(x)
def z():
    return y
aa = z()
if aa == '9':
    ab = aa + ' c1'
elif aa == '14':
    ab = aa + ' c2'
else:
    ab = aa + ' c3'
ac = ab[0:]
def ad():
    return ac
def ae():
    return ad()
def af():
    return ae()
ag = af()
def ah():
    return ag
def ai():
    return ah()
def aj():
    return ai()
ak = aj()
print(ak)