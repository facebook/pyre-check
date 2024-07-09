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
f = e + '.'
g = ''
for _ in range(3):
    for __ in range(4):
                g += f
h = ''
for _ in range(5):
        if _ == 1:
            break
        h += g
if h == h:
    k = h + 'c1'
elif h == '17':
    k = i + 'c2'
else:
    k = j + 'c3'
l = ''
for _ in range(3):
    l += k
m = l + '2'
n = m + '4'
o = ''
for _ in range(4):
    for __ in range(4):
                o += n
p = ''
for _ in range(10):
        if _ == 3:
            break
        p += o
q_dict = {55: p, 69: p, 49: p, 61: p}
r = random.choice(list(q_dict.values()))
s = [r for _ in range(8)]
random.shuffle(s)
t = random.choice(s)
u = t + '.'
v = [u for _ in range(5)]
random.shuffle(v)
w = random.choice(v)
x = ''
for _ in range(10):
        if _ == 1:
            break
        x += w
y = x + '7'
z = y + '2'
aa = z + '.'
ab_dict = {22: aa, 94: aa, 17: aa, 96: aa}
ac_dict = {30: ab_dict, 23: ab_dict}
ad = random.choice(list(ac_dict.values()))
ae = random.choice(list(ad.values()))
af_set = {ae, ae, ae, ae, ae, ae, ae}
af = random.choice(list(af_set))
print(af)