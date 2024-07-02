import random
import math
a = input()
b_list = [a for _ in range(7)]
c_list = [b_list for _ in range(3)]
d = random.choice(c_list)
e = random.choice(d)
f = e[0:]
g = f'string {f}'
h = [g for _ in range(9)]
random.shuffle(h)
i = random.choice(h)
if i == '5':
    j = i + ' c1'
elif i == '16':
    j = i + ' c2'
else:
    j = i + ' c3'
k_set = {j, j, j, j, j, j, j, j, j}
k = random.choice(list(k_set))
l = ''
counterl = 0
while counterl < 2:
    m = ''
    counterm = 0
    while counterm < 4:
        m += l
        counterm += 1
        l += k
        counterl += 1
n = [m for _ in range(9)]
random.shuffle(n)
o = random.choice(n)
p = f'string {o}'
q = ''
counterq = 0
while counterq < 3:
    r = ''
    counterr = 0
    while counterr < 4:
        s = ''
        counters = 0
        while counters < 2:
            s += r
            counters += 1
            r += q
            counterr += 1
        q += p
        counterq += 1
t_set = {s, s, s, s, s, s, s, s, s}
t = random.choice(list(t_set))
u = t + '4'
v = u + '4'
w = (v, v, v)
x, y, z = w
aa = x + y + z
ab_set = {aa, aa, aa}
ab = random.choice(list(ab_set))
ac = (ab, ab, ab)
ad, ae, af = ac
ag = ad + ae + af
ah = (ag, ag, ag)
ai, aj, ak = ah
al = ai + aj + ak
if al == '8':
    am = al + ' c1'
elif al == '15':
    am = al + ' c2'
else:
    am = al + ' c3'
an = am + '.'
print(an)