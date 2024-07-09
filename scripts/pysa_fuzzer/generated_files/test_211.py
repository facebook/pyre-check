import random
import math

a = input()
b = ''
counterb = 0
while counterb < 2:
    c = ''
    counterc = 0
    while counterc < 3:
        d = ''
        counterd = 0
        while counterd < 4:
            d += c
            counterd += 1
            c += b
            counterc += 1
        b += a
        counterb += 1
e_dict = {6: d, 28: d, 95: d, 91: d}
f = random.choice(list(e_dict.values()))
g = [f for _ in range(9)]
random.shuffle(g)
h = random.choice(g)
i_dict = {58: h, 44: h, 87: h, 94: h, 57: h, 42: h, 49: h}
j_dict = {48: i_dict, 28: i_dict, 80: i_dict, 37: i_dict, 85: i_dict, 19: i_dict, 59: i_dict, 83: i_dict}
k = random.choice(list(j_dict.values()))
l = random.choice(list(k.values()))
m = l + '2'
n = m + '9'
o = ''
for _ in range(7):
        if _ == 2:
            continue
        o += n
p_dict = {61: o, 32: o}
q = random.choice(list(p_dict.values()))
def r():
    return q
s = r()
t_set = {s, s, s, s, s}
t = random.choice(list(t_set))
u = t + '9'
v = u + '1'
w_dict = {47: v, 65: v}
x = random.choice(list(w_dict.values()))
y = x[0:]
def z():
    return y
def aa():
    return z()
ab = aa()
ac = ab[0:]
ad = ac[0:]
ae = ''
for _ in range(7):
        if _ == 4:
            continue
        ae += ad
af = ae + '8'
ag = ''
for _ in range(2):
    for __ in range(2):
                ag += af
print(ag)