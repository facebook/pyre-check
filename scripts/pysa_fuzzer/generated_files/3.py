import random
import math
a = input()
b = ''
for _ in range(2):
    c = ''
    for _ in range(2):
        d = ''
        for _ in range(2):
            d += c
            c += b
        b += a
e = d + '.'
f = e + '.'
def g():
    return f
def h():
    return g()
i = h()
j_list = [i for _ in range(3)]
k_list = [j_list for _ in range(6)]
l = random.choice(k_list)
m = random.choice(l)
n = f'string {m}'
o = ''
for _ in range(4):
    for __ in range(3):
                o += n
p = [o for _ in range(6)]
random.shuffle(p)
q = random.choice(p)
r = [q for _ in range(9)]
random.shuffle(r)
s = random.choice(r)
t = ''
for _ in range(2):
    t += s
u_dict = {95: t, 98: t, 11: t}
v = random.choice(list(u_dict.values()))
w = v[0:]
x = f'string {w}'
y = ''
countery = 0
while countery < 2:
    z = ''
    counterz = 0
    while counterz < 2:
        z += y
        counterz += 1
        y += x
        countery += 1
aa = ''
for _ in range(5):
        if _ == 3:
            break
        aa += z
ab = aa[0:]
ac = ab[0:]
ad_set = {ac, ac}
ad = random.choice(list(ad_set))
print(ad)