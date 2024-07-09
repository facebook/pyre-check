import random
import math

a = input()
b = ''
for _ in range(2):
    c = ''
    for _ in range(5):
        c += b
        b += a
d = [c for _ in range(8)]
random.shuffle(d)
e = random.choice(d)
f = ''
for _ in range(10):
        if _ == 2:
            break
        f += e
g_list = [f for _ in range(4)]
h = random.choice(g_list)
i = ''
for _ in range(3):
    for __ in range(4):
                i += h
j = i + '2'
def k():
    return j
l = k()
m = ''
for _ in range(4):
    m += l
n = ''
countern = 0
while countern < 4:
    o = ''
    countero = 0
    while countero < 5:
        p = ''
        counterp = 0
        while counterp < 5:
            p += o
            counterp += 1
            o += n
            countero += 1
        n += m
        countern += 1
q_list = [p for _ in range(3)]
r_list = [q_list for _ in range(8)]
s = random.choice(r_list)
t = random.choice(s)
u = ''
for _ in range(6):
        if _ == 2:
            continue
        u += t
v = u + '.'
if v == v:
    y = v + 'c1'
elif v == '11':
    y = w + 'c2'
else:
    y = x + 'c3'
z_list = [y for _ in range(10)]
aa = random.choice(z_list)
ab = ''
for _ in range(2):
    for __ in range(5):
                ab += aa
ac = ab + '.'
ad = (ac, ac, ac)
ae, af, ag = ad
ah = ae + af + ag
ai = ah[0:]
print(ai)