import random
import math

a = input()
b = ''
for _ in range(4):
    for __ in range(2):
                b += a
c = (b, b, b)
d, e, f = c
g = d + e + f
h = [g for _ in range(9)]
random.shuffle(h)
i = random.choice(h)
j = [i for _ in range(9)]
random.shuffle(j)
k = random.choice(j)
l_set = {k, k, k, k, k, k, k, k, k}
l = random.choice(list(l_set))
m = ''
for _ in range(5):
    for __ in range(2):
                m += l
def n():
    return m
def o():
    return n()
p = o()
q = ''
counterq = 0
while counterq < 4:
    r = ''
    counterr = 0
    while counterr < 3:
        r += q
        counterr += 1
        q += p
        counterq += 1
s = ''
for _ in range(7):
        if _ == 5:
            break
        s += r
t_list = [s for _ in range(9)]
u_list = [t_list for _ in range(6)]
v = random.choice(u_list)
w = random.choice(v)
x = w + '7'
y = x + '8'
z_set = {y, y}
z = random.choice(list(z_set))
aa = ''
for _ in range(3):
    for __ in range(4):
                aa += z
ab = f'string {aa}'
if ab == ab:
    ae = ab + 'c1'
elif ab == '12':
    ae = ac + 'c2'
else:
    ae = ad + 'c3'
af = f'string {ae}'
ag = f'string {af}'
ah = ag + '8'
print(ah)