import random
import math
a = input()
b = f'string {a}'
c = b + '.'
def d():
    return c
def e():
    return d()
f = e()
g_list = [f for _ in range(3)]
h_list = [g_list for _ in range(10)]
i_list = [h_list for _ in range(9)]
j = random.choice(i_list)
k = random.choice(j)
l = random.choice(k)
m = ''
counterm = 0
while counterm < 5:
    n = ''
    countern = 0
    while countern < 2:
        n += m
        countern += 1
        m += l
        counterm += 1
o = ''
for _ in range(10):
        if _ == 4:
            continue
        o += n
p = [o for _ in range(5)]
random.shuffle(p)
q = random.choice(p)
r = q + '6'
s = r + '4'
t = ''
for _ in range(9):
        if _ == 1:
            continue
        t += s
if t == '9':
    u = t + ' c1'
elif t == '16':
    u = t + ' c2'
else:
    u = t + ' c3'
v = u + '.'
w = ''
for _ in range(5):
    for __ in range(4):
                w += v
def x():
    return w
y = x()
z_list = [y for _ in range(4)]
aa = random.choice(z_list)
ab = ''
counterab = 0
while counterab < 5:
    ac = ''
    counterac = 0
    while counterac < 2:
        ac += ab
        counterac += 1
        ab += aa
        counterab += 1
ad_list = [ac for _ in range(5)]
ae = random.choice(ad_list)
if ae == '10':
    af = ae + ' c1'
elif ae == '12':
    af = ae + ' c2'
else:
    af = ae + ' c3'
ag = af + '5'
ah = ag + '7'
print(ah)