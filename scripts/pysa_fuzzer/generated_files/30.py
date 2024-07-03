import random
import math
a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g = (f, f, f)
h, i, j = g
k = h + i + j
l = ''
for _ in range(7):
        if _ == 4:
            break
        l += k
m_set = {l, l, l, l}
m = random.choice(list(m_set))
n = (m, m, m)
o, p, q = n
r = o + p + q
s_list = [r for _ in range(8)]
t_list = [s_list for _ in range(4)]
u = random.choice(t_list)
v = random.choice(u)
w = (v, v, v)
x, y, z = w
aa = x + y + z
ab = ''
for _ in range(5):
    for __ in range(3):
                ab += aa
ac = ab[0:]
ad = (ac, ac, ac)
ae, af, ag = ad
ah = ae + af + ag
def ai():
    return ah
def aj():
    return ai()
ak = aj()
al = (ak, ak, ak)
am, an, ao = al
ap = am + an + ao
aq = ''
for _ in range(4):
    for __ in range(2):
                aq += ap
ar = aq + '.'
at = ''
for _ in range(5):
        if _ == 2:
            break
        at += ar
au = f'string {at}'
av = (au, au, au)
aw, ax, ay = av
az = aw + ax + ay
ba = az + '.'
print(ba)