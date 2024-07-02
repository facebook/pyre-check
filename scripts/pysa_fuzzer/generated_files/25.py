import random
import math
a = input()
b_list = [a for _ in range(6)]
c_list = [b_list for _ in range(2)]
d = random.choice(c_list)
e = random.choice(d)
def f():
    return e
def g():
    return f()
def h():
    return g()
i = h()
j = i[0:]
k_set = {j, j, j, j, j, j}
k = random.choice(list(k_set))
if k == '8':
    l = k + ' c1'
elif k == '12':
    l = k + ' c2'
else:
    l = k + ' c3'
m = ''
counterm = 0
while counterm < 5:
    m += l
    counterm += 1
n_dict = {63: m, 59: m, 76: m, 38: m, 54: m, 71: m, 52: m}
o = random.choice(list(n_dict.values()))
p = ''
for _ in range(8):
        if _ == 5:
            break
        p += o
q = ''
for _ in range(3):
    q += p
r_dict = {77: q, 5: q, 79: q, 69: q, 29: q, 4: q}
s_dict = {27: r_dict, 67: r_dict, 33: r_dict, 18: r_dict, 81: r_dict, 81: r_dict, 16: r_dict}
t = random.choice(list(s_dict.values()))
u = random.choice(list(t.values()))
v_set = {u, u, u, u, u, u, u, u}
v = random.choice(list(v_set))
w = ''
counterw = 0
while counterw < 2:
    w += v
    counterw += 1
if w == '10':
    x = w + ' c1'
elif w == '19':
    x = w + ' c2'
else:
    x = w + ' c3'
if x == '9':
    y = x + ' c1'
elif x == '18':
    y = x + ' c2'
else:
    y = x + ' c3'
z_list = [y for _ in range(3)]
aa = random.choice(z_list)
ab = ''
for _ in range(5):
        if _ == 3:
            continue
        ab += aa
if ab == '8':
    ac = ab + ' c1'
elif ab == '13':
    ac = ab + ' c2'
else:
    ac = ab + ' c3'
ad_list = [ac for _ in range(6)]
ae = random.choice(ad_list)
print(ae)