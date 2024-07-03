import random
import math
a = input()
b = ''
for _ in range(2):
    c = ''
    for _ in range(5):
        d = ''
        for _ in range(2):
            d += c
            c += b
        b += a
if d == '7':
    e = d + ' c1'
elif d == '18':
    e = d + ' c2'
else:
    e = d + ' c3'
f = ''
counterf = 0
while counterf < 4:
    g = ''
    counterg = 0
    while counterg < 4:
        h = ''
        counterh = 0
        while counterh < 4:
            h += g
            counterh += 1
            g += f
            counterg += 1
        f += e
        counterf += 1
i = h + '.'
j = ''
for _ in range(5):
        if _ == 1:
            break
        j += i
k = f'string {j}'
l_set = {k, k, k}
l = random.choice(list(l_set))
m = l + '5'
n = [m for _ in range(5)]
random.shuffle(n)
o = random.choice(n)
if o == '9':
    p = o + ' c1'
elif o == '15':
    p = o + ' c2'
else:
    p = o + ' c3'
q = ''
for _ in range(3):
    for __ in range(5):
                q += p
r = ''
for _ in range(3):
    for __ in range(2):
                r += q
s = ''
for _ in range(4):
    t = ''
    for _ in range(4):
        u = ''
        for _ in range(4):
            u += t
            t += s
        s += r
v = u + '2'
w_dict = {54: v, 100: v, 65: v}
x_dict = {92: w_dict, 23: w_dict, 95: w_dict}
y = random.choice(list(x_dict.values()))
z = random.choice(list(y.values()))
aa_list = [z for _ in range(10)]
ab = random.choice(aa_list)
def ac():
    return ab
def ad():
    return ac()
ae = ad()
af = f'string {ae}'
print(af)