import random
import math
a = input()
b = a[0:]
c = f'string {b}'
def d():
    return c
def e():
    return d()
f = e()
g = ''
for _ in range(2):
    h = ''
    for _ in range(2):
        h += g
        g += f
i = h + '.'
j = f'string {i}'
k = j + '5'
l = k + '4'
m = l + '2'
n = ''
for _ in range(2):
    for __ in range(2):
                n += m
o = f'string {n}'
p = (o, o, o)
q, r, s = p
t = q + r + s
u = ''
for _ in range(5):
    u += t
v = u + '.'
w = ''
counterw = 0
while counterw < 3:
    w += v
    counterw += 1
x = ''
for _ in range(6):
        if _ == 5:
            continue
        x += w
y_dict = {88: x, 56: x, 6: x}
z_dict = {11: y_dict, 11: y_dict, 7: y_dict, 55: y_dict, 90: y_dict, 45: y_dict, 18: y_dict, 4: y_dict}
aa = random.choice(list(z_dict.values()))
ab = random.choice(list(aa.values()))
def ac():
    return ab
def ad():
    return ac()
ae = ad()
af_set = {ae, ae}
af = random.choice(list(af_set))
ag_set = {af, af, af, af, af}
ag = random.choice(list(ag_set))
print(ag)