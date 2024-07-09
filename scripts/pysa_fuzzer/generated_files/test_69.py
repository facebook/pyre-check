import random
import math

a = input()
b = ''
counterb = 0
while counterb < 5:
    c = ''
    counterc = 0
    while counterc < 5:
        c += b
        counterc += 1
        b += a
        counterb += 1
d = f'string {c}'
def e():
    return d
def f():
    return e()
g = f()
if g == g:
    j = g + 'c1'
elif g == '12':
    j = h + 'c2'
else:
    j = i + 'c3'
k = (j, j, j)
l, m, n = k
o = l + m + n
p_set = {o, o, o, o}
p = random.choice(list(p_set))
q = [p for _ in range(10)]
random.shuffle(q)
r = random.choice(q)
s = r + '.'
t_dict = {70: s, 32: s}
u_dict = {80: t_dict, 49: t_dict, 92: t_dict, 8: t_dict, 26: t_dict, 96: t_dict, 1: t_dict}
v = random.choice(list(u_dict.values()))
w = random.choice(list(v.values()))
x = w[0:]
y = ''
for _ in range(3):
    for __ in range(2):
                y += x
z = ''
for _ in range(5):
    aa = ''
    for _ in range(3):
        ab = ''
        for _ in range(4):
            ab += aa
            aa += z
        z += y
ac = ''
counterac = 0
while counterac < 3:
    ac += ab
    counterac += 1
ad = f'string {ac}'
ae = ad[0:]
af = ''
for _ in range(5):
        if _ == 5:
            break
        af += ae
ag = [af for _ in range(8)]
random.shuffle(ag)
ah = random.choice(ag)
ai = ah[0:]
print(ai)