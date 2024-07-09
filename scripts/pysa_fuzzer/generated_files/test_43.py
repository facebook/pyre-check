import random
import math

a = input()
b = ''
counterb = 0
while counterb < 3:
    b += a
    counterb += 1
c = ''
counterc = 0
while counterc < 2:
    c += b
    counterc += 1
d = (c, c, c)
e, f, g = d
h = e + f + g
i = ''
for _ in range(5):
    for __ in range(3):
                i += h
j = f'string {i}'
k = j + '1'
l_list = [k for _ in range(4)]
m_list = [l_list for _ in range(7)]
n_list = [m_list for _ in range(5)]
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
v = ''
for _ in range(5):
    w = ''
    for _ in range(2):
        w += v
        v += u
x = (w, w, w)
y, z, aa = x
ab = y + z + aa
ac = f'string {ab}'
ad = ac + '4'
ae = ad + '6'
af_list = [ae for _ in range(4)]
ag_list = [af_list for _ in range(6)]
ah = random.choice(ag_list)
ai = random.choice(ah)
aj = ''
for _ in range(6):
        if _ == 4:
            continue
        aj += ai
ak = aj + '.'
al = f'string {ak}'
am = al + '5'
an = am + '6'
ao = an + '3'
print(ao)