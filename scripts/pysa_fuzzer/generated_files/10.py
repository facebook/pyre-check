import random
import math
a = input()
b = ''
for _ in range(2):
    for __ in range(5):
                b += a
def c():
    return b
def d():
    return c()
e = d()
f = f'string {e}'
g = f + '.'
h = g + '2'
i = h + '7'
j = i + '5'
k_dict = {92: j, 27: j, 5: j}
l = random.choice(list(k_dict.values()))
m = ''
for _ in range(5):
    for __ in range(4):
                m += l
n = ''
countern = 0
while countern < 4:
    o = ''
    countero = 0
    while countero < 3:
        o += n
        countero += 1
        n += m
        countern += 1
p_set = {o, o, o, o, o}
p = random.choice(list(p_set))
q = ''
for _ in range(9):
        if _ == 5:
            continue
        q += p
r = q + '.'
s = ''
for _ in range(5):
        if _ == 5:
            continue
        s += r
t = [s for _ in range(9)]
random.shuffle(t)
u = random.choice(t)
v = [u for _ in range(8)]
random.shuffle(v)
w = random.choice(v)
x = w + '.'
y = ''
for _ in range(6):
        if _ == 3:
            break
        y += x
if y == '4':
    z = y + ' c1'
elif y == '19':
    z = y + ' c2'
else:
    z = y + ' c3'
aa_set = {z, z, z, z, z}
aa = random.choice(list(aa_set))
print(aa)