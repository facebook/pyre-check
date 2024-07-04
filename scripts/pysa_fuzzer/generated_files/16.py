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
for _ in range(6):
        if _ == 2:
            break
        l += k
if l == '8':
    m = l + ' c1'
elif l == '15':
    m = l + ' c2'
else:
    m = l + ' c3'
n_set = {m, m, m}
n = random.choice(list(n_set))
o = [n for _ in range(6)]
random.shuffle(o)
p = random.choice(o)
q = p[0:]
r = q[0:]
s_list = [r for _ in range(8)]
t_list = [s_list for _ in range(5)]
u = random.choice(t_list)
v = random.choice(u)
w_dict = {68: v, 44: v, 59: v}
x = random.choice(list(w_dict.values()))
y_set = {x, x, x}
y = random.choice(list(y_set))
z = ''
for _ in range(5):
        if _ == 4:
            continue
        z += y
aa = z + '1'
ab = aa + '2'
ac_set = {ab, ab, ab, ab, ab}
ac = random.choice(list(ac_set))
ad = ac + '1'
ae = ad + '8'
af = ae + '4'
ag_list = [af for _ in range(2)]
ah_list = [ag_list for _ in range(8)]
ai = random.choice(ah_list)
aj = random.choice(ai)
ak = [aj for _ in range(8)]
random.shuffle(ak)
al = random.choice(ak)
if al == '6':
    am = al + ' c1'
elif al == '11':
    am = al + ' c2'
else:
    am = al + ' c3'
print(am)