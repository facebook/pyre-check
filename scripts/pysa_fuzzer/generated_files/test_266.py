import random
import math

a = input()
b = ''
counterb = 0
while counterb < 3:
    b += a
    counterb += 1
c = f'string {b}'
d_set = {c, c, c, c, c, c}
d = random.choice(list(d_set))
e_dict = {76: d, 88: d, 31: d, 45: d, 51: d, 29: d}
f = random.choice(list(e_dict.values()))
g = [f for _ in range(5)]
random.shuffle(g)
h = random.choice(g)
i_list = [h for _ in range(4)]
j = random.choice(i_list)
k = [j for _ in range(6)]
random.shuffle(k)
l = random.choice(k)
m_set = {l, l, l, l, l, l, l, l, l}
m = random.choice(list(m_set))
n = ''
for _ in range(5):
    o = ''
    for _ in range(3):
        p = ''
        for _ in range(5):
            p += o
            o += n
        n += m
if p == p:
    s = p + 'c1'
elif p == '11':
    s = q + 'c2'
else:
    s = r + 'c3'
t = ''
countert = 0
while countert < 2:
    t += s
    countert += 1
u = t + '.'
v = ''
for _ in range(2):
    for __ in range(5):
                v += u
w = ''
for _ in range(2):
    w += v
x = ''
for _ in range(3):
    y = ''
    for _ in range(2):
        z = ''
        for _ in range(5):
            z += y
            y += x
        x += w
aa = ''
for _ in range(8):
        if _ == 2:
            break
        aa += z
ab_list = [aa for _ in range(3)]
ac_list = [ab_list for _ in range(3)]
ad = random.choice(ac_list)
ae = random.choice(ad)
if ae == ae:
    ah = ae + 'c1'
elif ae == '12':
    ah = af + 'c2'
else:
    ah = ag + 'c3'
print(ah)