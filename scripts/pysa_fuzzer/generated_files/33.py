import random
import math
a = input()
b = a[0:]
c = b + '.'
d = (c, c, c)
e, f, g = d
h = e + f + g
i = h + '.'
j = f'string {i}'
k = (j, j, j)
l, m, n = k
o = l + m + n
p = f'string {o}'
q = (p, p, p)
r, s, t = q
u = r + s + t
v = f'string {u}'
w = v + '9'
x = w + '3'
y = (x, x, x)
z, aa, ab = y
ac = z + aa + ab
ad_dict = {87: ac, 42: ac, 13: ac, 6: ac, 25: ac, 60: ac, 51: ac, 39: ac, 6: ac, 25: ac}
ae_dict = {33: ad_dict, 76: ad_dict, 57: ad_dict, 42: ad_dict}
af = random.choice(list(ae_dict.values()))
ag = random.choice(list(af.values()))
ah_list = [ag for _ in range(6)]
ai_list = [ah_list for _ in range(2)]
aj_list = [ai_list for _ in range(5)]
ak = random.choice(aj_list)
al = random.choice(ak)
am = random.choice(al)
def an():
    return am
ao = an()
ap = f'string {ao}'
aq = ap + '5'
ar = aq + '7'
at = ar + '2'
au = at + '2'
av = au + '5'
aw = ''
counteraw = 0
while counteraw < 2:
    ax = ''
    counterax = 0
    while counterax < 3:
        ax += aw
        counterax += 1
        aw += av
        counteraw += 1
print(ax)