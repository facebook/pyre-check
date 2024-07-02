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
f = e[0:]
g_list = [f for _ in range(3)]
h = random.choice(g_list)
i = (h, h, h)
j, k, l = i
m = j + k + l
n = ''
for _ in range(5):
        if _ == 4:
            continue
        n += m
o = ''
for _ in range(2):
    for __ in range(3):
                o += n
if o == '6':
    p = o + ' c1'
elif o == '15':
    p = o + ' c2'
else:
    p = o + ' c3'
q = ''
counterq = 0
while counterq < 3:
    r = ''
    counterr = 0
    while counterr < 4:
        s = ''
        counters = 0
        while counters < 3:
            s += r
            counters += 1
            r += q
            counterr += 1
        q += p
        counterq += 1
t = f'string {s}'
u = ''
for _ in range(3):
    for __ in range(4):
                u += t
v = f'string {u}'
w_set = {v, v, v, v, v, v, v}
w = random.choice(list(w_set))
x = w + '4'
y = x + '4'
z = y + '4'
aa = ''
counteraa = 0
while counteraa < 5:
    ab = ''
    counterab = 0
    while counterab < 3:
        ac = ''
        counterac = 0
        while counterac < 5:
            ac += ab
            counterac += 1
            ab += aa
            counterab += 1
        aa += z
        counteraa += 1
ad = ac + '9'
ae = ''
for _ in range(2):
    for __ in range(4):
                ae += ad
af = ''
for _ in range(5):
    af += ae
ag = f'string {af}'
print(ag)