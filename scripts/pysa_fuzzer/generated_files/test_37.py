import random
import math

a = input()
b_dict = {56: a, 93: a, 29: a}
c_dict = {21: b_dict, 4: b_dict, 73: b_dict, 29: b_dict, 83: b_dict, 12: b_dict, 19: b_dict, 39: b_dict}
d = random.choice(list(c_dict.values()))
e = random.choice(list(d.values()))
f = ''
for _ in range(4):
    for __ in range(4):
                f += e
g = ''
for _ in range(2):
    h = ''
    for _ in range(5):
        h += g
        g += f
i = (h, h, h)
j, k, l = i
m = j + k + l
n = ''
for _ in range(5):
    for __ in range(2):
                n += m
o = n + '2'
p = o + '3'
q = p + '.'
r = ''
for _ in range(3):
    r += q
s_list = [r for _ in range(7)]
t_list = [s_list for _ in range(10)]
u = random.choice(t_list)
v = random.choice(u)
w = ''
counterw = 0
while counterw < 3:
    x = ''
    counterx = 0
    while counterx < 3:
        y = ''
        countery = 0
        while countery < 3:
            y += x
            countery += 1
            x += w
            counterx += 1
        w += v
        counterw += 1
z = ''
for _ in range(3):
    aa = ''
    for _ in range(3):
        aa += z
        z += y
ab = aa[0:]
ac = ''
for _ in range(3):
    for __ in range(3):
                ac += ab
ad = f'string {ac}'
ae = ''
for _ in range(5):
    for __ in range(4):
                ae += ad
af = ae + '2'
ag = af + '7'
ah_set = {ag, ag, ag, ag, ag, ag, ag, ag}
ah = random.choice(list(ah_set))
ai = ah + '7'
print(ai)