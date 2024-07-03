import random
import math
a = input()
b = a + '1'
c = b + '9'
d = f'string {c}'
e = d[0:]
f = ''
for _ in range(4):
    for __ in range(4):
                f += e
if f == '2':
    g = f + ' c1'
elif f == '12':
    g = f + ' c2'
else:
    g = f + ' c3'
h = g[0:]
i = (h, h, h)
j, k, l = i
m = j + k + l
n = (m, m, m)
o, p, q = n
r = o + p + q
s = [r for _ in range(10)]
random.shuffle(s)
t = random.choice(s)
if t == '5':
    u = t + ' c1'
elif t == '15':
    u = t + ' c2'
else:
    u = t + ' c3'
v = [u for _ in range(6)]
random.shuffle(v)
w = random.choice(v)
def x():
    return w
y = x()
z = ''
counterz = 0
while counterz < 5:
    aa = ''
    counteraa = 0
    while counteraa < 3:
        aa += z
        counteraa += 1
        z += y
        counterz += 1
ab = f'string {aa}'
ac = ab + '.'
if ac == '9':
    ad = ac + ' c1'
elif ac == '14':
    ad = ac + ' c2'
else:
    ad = ac + ' c3'
ae = ''
for _ in range(3):
    af = ''
    for _ in range(5):
        af += ae
        ae += ad
ag_list = [af for _ in range(8)]
ah_list = [ag_list for _ in range(6)]
ai_list = [ah_list for _ in range(10)]
aj = random.choice(ai_list)
ak = random.choice(aj)
al = random.choice(ak)
print(al)