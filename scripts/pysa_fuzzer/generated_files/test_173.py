import random
import math

a = input()
b = [a for _ in range(6)]
random.shuffle(b)
c = random.choice(b)
d = ''
counterd = 0
while counterd < 5:
    e = ''
    countere = 0
    while countere < 3:
        f = ''
        counterf = 0
        while counterf < 2:
            f += e
            counterf += 1
            e += d
            countere += 1
        d += c
        counterd += 1
def g():
    return f
h = g()
i = [h for _ in range(7)]
random.shuffle(i)
j = random.choice(i)
k_set = {j, j, j, j, j}
k = random.choice(list(k_set))
l_list = [k for _ in range(10)]
m_list = [l_list for _ in range(8)]
n_list = [m_list for _ in range(9)]
o = random.choice(n_list)
p = random.choice(o)
q = random.choice(p)
r = q[0:]
if r == r:
    u = r + 'c1'
elif r == '14':
    u = s + 'c2'
else:
    u = t + 'c3'
v = u + '9'
w_set = {v, v}
w = random.choice(list(w_set))
if w == w:
    z = w + 'c1'
elif w == '17':
    z = x + 'c2'
else:
    z = y + 'c3'
aa = z + '8'
ab = aa + '2'
ac = ab[0:]
ad = ''
for _ in range(4):
    ad += ac
ae = ''
for _ in range(4):
    for __ in range(4):
                ae += ad
af = ae + '.'
ag_set = {af, af, af, af, af, af, af, af, af}
ag = random.choice(list(ag_set))
def ah():
    return ag
def ai():
    return ah()
aj = ai()
print(aj)