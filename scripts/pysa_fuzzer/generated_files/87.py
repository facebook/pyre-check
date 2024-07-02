import random
import math
a = input()
b_set = {a, a, a, a, a}
b = random.choice(list(b_set))
c = ''
for _ in range(5):
        if _ == 2:
            continue
        c += b
d = c + '8'
e = ''
for _ in range(10):
        if _ == 3:
            break
        e += d
f = ''
for _ in range(4):
    g = ''
    for _ in range(5):
        h = ''
        for _ in range(2):
            h += g
            g += f
        f += e
i = h + '5'
j = i + '7'
k = j + '3'
l_list = [k for _ in range(10)]
m = random.choice(l_list)
n_set = {m, m, m, m}
n = random.choice(list(n_set))
o = n + '6'
p = o + '6'
q = p + '3'
r = q + '6'
s_set = {r, r, r, r}
s = random.choice(list(s_set))
def t():
    return s
def u():
    return t()
def v():
    return u()
w = v()
x = f'string {w}'
y = f'string {x}'
z = ''
for _ in range(2):
    for __ in range(5):
                z += y
aa_dict = {84: z, 54: z, 2: z, 75: z}
ab_dict = {48: aa_dict, 17: aa_dict, 13: aa_dict, 83: aa_dict, 15: aa_dict, 89: aa_dict, 47: aa_dict, 72: aa_dict, 82: aa_dict, 14: aa_dict}
ac = random.choice(list(ab_dict.values()))
ad = random.choice(list(ac.values()))
ae = ad[0:]
def af():
    return ae
def ag():
    return af()
def ah():
    return ag()
ai = ah()
print(ai)