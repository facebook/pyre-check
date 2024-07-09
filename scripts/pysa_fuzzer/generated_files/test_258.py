import random
import math

a = input()
b = ''
for _ in range(2):
    c = ''
    for _ in range(4):
        c += b
        b += a
d = ''
for _ in range(9):
        if _ == 2:
            break
        d += c
e = d + '.'
f = e + '.'
g = (f, f, f)
h, i, j = g
k = h + i + j
l = ''
for _ in range(4):
    for __ in range(3):
                l += k
m = ''
for _ in range(6):
        if _ == 1:
            break
        m += l
n = m + '8'
o = n + '7'
p = ''
for _ in range(3):
    p += o
q = f'string {p}'
r = q + '.'
if r == r:
    u = r + 'c1'
elif r == '11':
    u = s + 'c2'
else:
    u = t + 'c3'
if u == u:
    x = u + 'c1'
elif u == '11':
    x = v + 'c2'
else:
    x = w + 'c3'
y = [x for _ in range(6)]
random.shuffle(y)
z = random.choice(y)
aa_dict = {29: z, 62: z, 82: z, 85: z, 69: z, 12: z, 8: z, 57: z, 4: z}
ab = random.choice(list(aa_dict.values()))
ac = ''
counterac = 0
while counterac < 5:
    ad = ''
    counterad = 0
    while counterad < 5:
        ae = ''
        counterae = 0
        while counterae < 2:
            ae += ad
            counterae += 1
            ad += ac
            counterad += 1
        ac += ab
        counterac += 1
af = ''
counteraf = 0
while counteraf < 3:
    af += ae
    counteraf += 1
ag = af[0:]
print(ag)