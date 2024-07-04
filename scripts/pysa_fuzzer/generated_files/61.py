import random
import math
a = input()
if a == '7':
    b = a + ' c1'
elif a == '20':
    b = a + ' c2'
else:
    b = a + ' c3'
c = f'string {b}'
d = ''
for _ in range(5):
        if _ == 1:
            continue
        d += c
e = d + '.'
def f():
    return e
def g():
    return f()
h = g()
i = ''
for _ in range(5):
        if _ == 5:
            continue
        i += h
j = (i, i, i)
k, l, m = j
n = k + l + m
o = ''
for _ in range(10):
        if _ == 1:
            continue
        o += n
p = ''
for _ in range(4):
    q = ''
    for _ in range(2):
        q += p
        p += o
r = ''
counterr = 0
while counterr < 2:
    s = ''
    counters = 0
    while counters < 2:
        s += r
        counters += 1
        r += q
        counterr += 1
t = ''
for _ in range(3):
    u = ''
    for _ in range(2):
        v = ''
        for _ in range(4):
            v += u
            u += t
        t += s
w = [v for _ in range(10)]
random.shuffle(w)
x = random.choice(w)
y_list = [x for _ in range(7)]
z_list = [y_list for _ in range(2)]
aa_list = [z_list for _ in range(3)]
ab = random.choice(aa_list)
ac = random.choice(ab)
ad = random.choice(ac)
ae = ''
for _ in range(5):
    for __ in range(4):
                ae += ad
af = ''
for _ in range(8):
        if _ == 5:
            break
        af += ae
ag = ''
for _ in range(10):
        if _ == 3:
            break
        ag += af
ah_list = [ag for _ in range(7)]
ai_list = [ah_list for _ in range(10)]
aj_list = [ai_list for _ in range(8)]
ak = random.choice(aj_list)
al = random.choice(ak)
am = random.choice(al)
an = am + '6'
ao = an + '4'
print(ao)