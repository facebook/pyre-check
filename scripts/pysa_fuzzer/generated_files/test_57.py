import random
import math

a = input()
def b():
    return a
def c():
    return b()
def d():
    return c()
e = d()
f = ''
counterf = 0
while counterf < 4:
    f += e
    counterf += 1
g = ''
for _ in range(4):
    h = ''
    for _ in range(2):
        h += g
        g += f
i_set = {h, h, h, h, h, h, h, h, h}
i = random.choice(list(i_set))
j = i + '5'
k = j + '.'
l_list = [k for _ in range(4)]
m_list = [l_list for _ in range(6)]
n_list = [m_list for _ in range(7)]
o = random.choice(n_list)
p = random.choice(o)
q = random.choice(p)
r = ''
for _ in range(10):
        if _ == 5:
            continue
        r += q
s = f'string {r}'
t = s + '.'
u = [t for _ in range(5)]
random.shuffle(u)
v = random.choice(u)
w = ''
counterw = 0
while counterw < 2:
    w += v
    counterw += 1
x_set = {w, w, w, w, w, w}
x = random.choice(list(x_set))
y = ''
for _ in range(4):
    for __ in range(4):
                y += x
z = ''
for _ in range(3):
    aa = ''
    for _ in range(4):
        ab = ''
        for _ in range(5):
            ab += aa
            aa += z
        z += y
ac = ''
counterac = 0
while counterac < 2:
    ad = ''
    counterad = 0
    while counterad < 3:
        ad += ac
        counterad += 1
        ac += ab
        counterac += 1
ae = ''
counterae = 0
while counterae < 2:
    af = ''
    counteraf = 0
    while counteraf < 5:
        af += ae
        counteraf += 1
        ae += ad
        counterae += 1
ag = [af for _ in range(6)]
random.shuffle(ag)
ah = random.choice(ag)
print(ah)