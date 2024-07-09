import random
import math

a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g = ''
for _ in range(10):
        if _ == 4:
            continue
        g += f
h = g + '.'
i = h[0:]
j = [i for _ in range(7)]
random.shuffle(j)
k = random.choice(j)
l = k + '.'
m = ''
for _ in range(5):
        if _ == 3:
            break
        m += l
n = ''
for _ in range(5):
    for __ in range(4):
                n += m
o = ''
countero = 0
while countero < 2:
    p = ''
    counterp = 0
    while counterp < 3:
        p += o
        counterp += 1
        o += n
        countero += 1
q = ''
counterq = 0
while counterq < 5:
    q += p
    counterq += 1
r = q + '.'
s = [r for _ in range(6)]
random.shuffle(s)
t = random.choice(s)
u = ''
for _ in range(5):
    v = ''
    for _ in range(3):
        v += u
        u += t
w = ''
counterw = 0
while counterw < 5:
    w += v
    counterw += 1
x = w + '2'
y = x + '1'
z = y + '8'
aa_dict = {56: z, 35: z, 65: z, 55: z, 2: z}
ab = random.choice(list(aa_dict.values()))
ac = ''
for _ in range(5):
    for __ in range(5):
                ac += ab
ad = ac + '.'
print(ad)