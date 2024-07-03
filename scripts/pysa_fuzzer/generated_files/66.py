import random
import math
a = input()
def b():
    return a
c = b()
d_set = {c, c}
d = random.choice(list(d_set))
e_set = {d, d, d, d, d, d, d}
e = random.choice(list(e_set))
if e == '4':
    f = e + ' c1'
elif e == '11':
    f = e + ' c2'
else:
    f = e + ' c3'
if f == '1':
    g = f + ' c1'
elif f == '19':
    g = f + ' c2'
else:
    g = f + ' c3'
def h():
    return g
def i():
    return h()
def j():
    return i()
k = j()
l = ''
for _ in range(7):
        if _ == 5:
            break
        l += k
m = ''
counterm = 0
while counterm < 2:
    n = ''
    countern = 0
    while countern < 5:
        n += m
        countern += 1
        m += l
        counterm += 1
o = f'string {n}'
p_set = {o, o, o, o}
p = random.choice(list(p_set))
q_list = [p for _ in range(6)]
r_list = [q_list for _ in range(5)]
s = random.choice(r_list)
t = random.choice(s)
u = ''
for _ in range(6):
        if _ == 3:
            break
        u += t
v = ''
for _ in range(7):
        if _ == 2:
            break
        v += u
w = v + '.'
x_set = {w, w, w}
x = random.choice(list(x_set))
y_set = {x, x, x, x, x, x, x, x, x}
y = random.choice(list(y_set))
z = ''
for _ in range(2):
    aa = ''
    for _ in range(5):
        ab = ''
        for _ in range(4):
            ab += aa
            aa += z
        z += y
ac_list = [ab for _ in range(8)]
ad_list = [ac_list for _ in range(10)]
ae_list = [ad_list for _ in range(4)]
af = random.choice(ae_list)
ag = random.choice(af)
ah = random.choice(ag)
print(ah)