import random
import math
a = input()
b = a[0:]
c_set = {b, b, b, b, b, b, b, b, b, b}
c = random.choice(list(c_set))
d = [c for _ in range(7)]
random.shuffle(d)
e = random.choice(d)
if e == '1':
    f = e + ' c1'
elif e == '12':
    f = e + ' c2'
else:
    f = e + ' c3'
def g():
    return f
def h():
    return g()
i = h()
j = i + '9'
k = j + '1'
l = k + '4'
m = (l, l, l)
n, o, p = m
q = n + o + p
if q == '7':
    r = q + ' c1'
elif q == '11':
    r = q + ' c2'
else:
    r = q + ' c3'
s_dict = {7: r, 56: r}
t = random.choice(list(s_dict.values()))
u_list = [t for _ in range(6)]
v = random.choice(u_list)
w = (v, v, v)
x, y, z = w
aa = x + y + z
ab_list = [aa for _ in range(4)]
ac_list = [ab_list for _ in range(2)]
ad = random.choice(ac_list)
ae = random.choice(ad)
af = (ae, ae, ae)
ag, ah, ai = af
aj = ag + ah + ai
ak = f'string {aj}'
al = ''
counteral = 0
while counteral < 3:
    al += ak
    counteral += 1
am = ''
for _ in range(6):
        if _ == 5:
            continue
        am += al
an = [am for _ in range(10)]
random.shuffle(an)
ao = random.choice(an)
ap = ao + '6'
print(ap)