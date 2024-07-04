import random
import math
a = input()
b = a[0:]
c = ''
for _ in range(4):
    c += b
d = ''
for _ in range(3):
    for __ in range(5):
                d += c
e = ''
for _ in range(3):
    f = ''
    for _ in range(5):
        g = ''
        for _ in range(2):
            g += f
            f += e
        e += d
h = [g for _ in range(9)]
random.shuffle(h)
i = random.choice(h)
if i == '8':
    j = i + ' c1'
elif i == '16':
    j = i + ' c2'
else:
    j = i + ' c3'
k = f'string {j}'
def l():
    return k
def m():
    return l()
n = m()
o = ''
for _ in range(6):
        if _ == 2:
            break
        o += n
p_dict = {46: o, 43: o, 23: o, 64: o}
q_dict = {55: p_dict, 74: p_dict, 96: p_dict, 40: p_dict, 74: p_dict, 84: p_dict, 57: p_dict, 58: p_dict}
r_dict = {8: q_dict, 67: q_dict, 2: q_dict, 44: q_dict, 23: q_dict}
s = random.choice(list(r_dict.values()))
t = random.choice(list(s.values()))
u = random.choice(list(t.values()))
v = ''
counterv = 0
while counterv < 3:
    w = ''
    counterw = 0
    while counterw < 3:
        w += v
        counterw += 1
        v += u
        counterv += 1
if w == '1':
    x = w + ' c1'
elif w == '12':
    x = w + ' c2'
else:
    x = w + ' c3'
y = f'string {x}'
if y == '8':
    z = y + ' c1'
elif y == '16':
    z = y + ' c2'
else:
    z = y + ' c3'
aa_set = {z, z, z}
aa = random.choice(list(aa_set))
ab = ''
for _ in range(3):
    for __ in range(4):
                ab += aa
ac = f'string {ab}'
ad = ''
for _ in range(6):
        if _ == 4:
            continue
        ad += ac
print(ad)