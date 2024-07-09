import random
import math

a = input()
b = a + '9'
c = b + '.'
d_dict = {64: c, 74: c, 89: c, 81: c, 41: c, 71: c, 24: c, 77: c, 51: c, 54: c}
e_dict = {11: d_dict, 45: d_dict, 33: d_dict, 78: d_dict, 28: d_dict, 32: d_dict}
f_dict = {87: e_dict, 61: e_dict, 37: e_dict, 25: e_dict, 98: e_dict, 66: e_dict, 10: e_dict}
g = random.choice(list(f_dict.values()))
h = random.choice(list(g.values()))
i = random.choice(list(h.values()))
j = ''
for _ in range(5):
    k = ''
    for _ in range(5):
        l = ''
        for _ in range(3):
            l += k
            k += j
        j += i
m = l + '.'
if m == m:
    p = m + 'c1'
elif m == '11':
    p = n + 'c2'
else:
    p = o + 'c3'
q = p[0:]
r = ''
for _ in range(3):
    for __ in range(5):
                r += q
s = [r for _ in range(7)]
random.shuffle(s)
t = random.choice(s)
u = ''
for _ in range(5):
    for __ in range(3):
                u += t
v = ''
counterv = 0
while counterv < 5:
    v += u
    counterv += 1
w = f'string {v}'
x_set = {w, w, w, w, w, w, w, w}
x = random.choice(list(x_set))
y = f'string {x}'
z = ''
counterz = 0
while counterz < 4:
    aa = ''
    counteraa = 0
    while counteraa < 5:
        aa += z
        counteraa += 1
        z += y
        counterz += 1
ab = ''
for _ in range(4):
    ac = ''
    for _ in range(3):
        ac += ab
        ab += aa
ad = ''
for _ in range(7):
        if _ == 3:
            continue
        ad += ac
ae = (ad, ad, ad)
af, ag, ah = ae
ai = af + ag + ah
print(ai)