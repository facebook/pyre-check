import random
import math
a = input()
b = ''
for _ in range(3):
    for __ in range(4):
                b += a
if b == '9':
    c = b + ' c1'
elif b == '20':
    c = b + ' c2'
else:
    c = b + ' c3'
d = c + '1'
e = d + '3'
f = e + '3'
g = ''
for _ in range(5):
    for __ in range(2):
                g += f
h = g + '3'
i = h + '8'
j_dict = {56: i, 59: i, 38: i, 5: i, 36: i, 67: i, 35: i, 77: i}
k_dict = {37: j_dict, 35: j_dict, 81: j_dict, 73: j_dict, 13: j_dict, 87: j_dict, 93: j_dict}
l_dict = {35: k_dict, 8: k_dict, 89: k_dict, 3: k_dict, 21: k_dict, 63: k_dict}
m = random.choice(list(l_dict.values()))
n = random.choice(list(m.values()))
o = random.choice(list(n.values()))
p = f'string {o}'
q = p + '.'
r = ''
for _ in range(2):
    r += q
def s():
    return r
def t():
    return s()
def u():
    return t()
v = u()
w_set = {v, v, v, v, v, v, v, v, v, v}
w = random.choice(list(w_set))
if w == '9':
    x = w + ' c1'
elif w == '16':
    x = w + ' c2'
else:
    x = w + ' c3'
if x == '9':
    y = x + ' c1'
elif x == '20':
    y = x + ' c2'
else:
    y = x + ' c3'
z = [y for _ in range(9)]
random.shuffle(z)
aa = random.choice(z)
ab = f'string {aa}'
ac_dict = {93: ab, 81: ab, 25: ab, 81: ab, 5: ab, 46: ab}
ad = random.choice(list(ac_dict.values()))
ae = ad + '4'
af = ae + '9'
ag = af + '5'
ah = [ag for _ in range(8)]
random.shuffle(ah)
ai = random.choice(ah)
print(ai)