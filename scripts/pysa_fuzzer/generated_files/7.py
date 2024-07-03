import random
import math
a = input()
b = a + '.'
c = b[0:]
d = [c for _ in range(7)]
random.shuffle(d)
e = random.choice(d)
if e == '8':
    f = e + ' c1'
elif e == '20':
    f = e + ' c2'
else:
    f = e + ' c3'
g = ''
for _ in range(4):
    for __ in range(5):
                g += f
h = ''
for _ in range(8):
        if _ == 3:
            continue
        h += g
i = ''
for _ in range(2):
    j = ''
    for _ in range(3):
        j += i
        i += h
k = ''
counterk = 0
while counterk < 5:
    l = ''
    counterl = 0
    while counterl < 3:
        l += k
        counterl += 1
        k += j
        counterk += 1
def m():
    return l
def n():
    return m()
o = n()
p = [o for _ in range(7)]
random.shuffle(p)
q = random.choice(p)
r = ''
counterr = 0
while counterr < 3:
    s = ''
    counters = 0
    while counters < 4:
        s += r
        counters += 1
        r += q
        counterr += 1
t_set = {s, s, s, s, s, s, s, s, s, s}
t = random.choice(list(t_set))
def u():
    return t
def v():
    return u()
w = v()
if w == '2':
    x = w + ' c1'
elif w == '14':
    x = w + ' c2'
else:
    x = w + ' c3'
y_list = [x for _ in range(3)]
z_list = [y_list for _ in range(9)]
aa = random.choice(z_list)
ab = random.choice(aa)
ac = [ab for _ in range(8)]
random.shuffle(ac)
ad = random.choice(ac)
ae = ''
for _ in range(5):
    for __ in range(4):
                ae += ad
af_dict = {51: ae, 20: ae, 74: ae, 97: ae, 48: ae}
ag = random.choice(list(af_dict.values()))
print(ag)