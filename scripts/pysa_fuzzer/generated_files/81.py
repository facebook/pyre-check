import random
import math
a = input()
b = ''
counterb = 0
while counterb < 2:
    b += a
    counterb += 1
c = ''
for _ in range(3):
    for __ in range(4):
                c += b
d_dict = {74: c, 59: c, 83: c, 85: c}
e_dict = {60: d_dict, 91: d_dict, 12: d_dict}
f = random.choice(list(e_dict.values()))
g = random.choice(list(f.values()))
h = f'string {g}'
i = [h for _ in range(5)]
random.shuffle(i)
j = random.choice(i)
k_set = {j, j, j, j, j, j, j, j, j}
k = random.choice(list(k_set))
l = [k for _ in range(9)]
random.shuffle(l)
m = random.choice(l)
n = m + '3'
o = n + '9'
p = o + '5'
q = ''
for _ in range(10):
        if _ == 1:
            break
        q += p
r = q + '1'
s = r + '4'
t = (s, s, s)
u, v, w = t
x = u + v + w
if x == '4':
    y = x + ' c1'
elif x == '15':
    y = x + ' c2'
else:
    y = x + ' c3'
z = ''
for _ in range(2):
    z += y
aa = ''
for _ in range(3):
    aa += z
ab = aa + '.'
ac = [ab for _ in range(7)]
random.shuffle(ac)
ad = random.choice(ac)
if ad == '7':
    ae = ad + ' c1'
elif ad == '17':
    ae = ad + ' c2'
else:
    ae = ad + ' c3'
af = [ae for _ in range(5)]
random.shuffle(af)
ag = random.choice(af)
print(ag)