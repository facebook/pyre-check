import random
import math

a = input()
b_list = [a for _ in range(9)]
c = random.choice(b_list)
d = ''
counterd = 0
while counterd < 4:
    e = ''
    countere = 0
    while countere < 5:
        e += d
        countere += 1
        d += c
        counterd += 1
f = ''
for _ in range(2):
    g = ''
    for _ in range(2):
        g += f
        f += e
h = [g for _ in range(9)]
random.shuffle(h)
i = random.choice(h)
j = ''
for _ in range(6):
        if _ == 4:
            break
        j += i
k_set = {j, j}
k = random.choice(list(k_set))
l = k[0:]
if l == l:
    o = l + 'c1'
elif l == '19':
    o = m + 'c2'
else:
    o = n + 'c3'
p_dict = {95: o, 43: o, 69: o, 24: o, 47: o, 85: o, 61: o, 34: o}
q_dict = {19: p_dict, 90: p_dict, 73: p_dict, 79: p_dict, 88: p_dict, 99: p_dict, 78: p_dict, 43: p_dict, 51: p_dict, 44: p_dict}
r = random.choice(list(q_dict.values()))
s = random.choice(list(r.values()))
t = ''
for _ in range(5):
    u = ''
    for _ in range(3):
        u += t
        t += s
v = ''
for _ in range(3):
    for __ in range(3):
                v += u
w = v + '.'
x = w + '.'
y = ''
countery = 0
while countery < 3:
    z = ''
    counterz = 0
    while counterz < 4:
        aa = ''
        counteraa = 0
        while counteraa < 2:
            aa += z
            counteraa += 1
            z += y
            counterz += 1
        y += x
        countery += 1
ab = f'string {aa}'
ac = ''
for _ in range(8):
        if _ == 1:
            continue
        ac += ab
ad = f'string {ac}'
ae = ad + '2'
print(ae)