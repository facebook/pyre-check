import random
import math
a = input()
b = a + '.'
c_set = {b, b, b, b, b, b, b, b}
c = random.choice(list(c_set))
d = c + '.'
e = d[0:]
f = ''
for _ in range(5):
    for __ in range(5):
                f += e
g_set = {f, f, f, f, f, f, f}
g = random.choice(list(g_set))
h = f'string {g}'
if h == '3':
    i = h + ' c1'
elif h == '20':
    i = h + ' c2'
else:
    i = h + ' c3'
if i == '10':
    j = i + ' c1'
elif i == '17':
    j = i + ' c2'
else:
    j = i + ' c3'
k = ''
counterk = 0
while counterk < 3:
    k += j
    counterk += 1
l = k + '.'
m = (l, l, l)
n, o, p = m
q = n + o + p
r = (q, q, q)
s, t, u = r
v = s + t + u
w_list = [v for _ in range(10)]
x_list = [w_list for _ in range(10)]
y = random.choice(x_list)
z = random.choice(y)
aa = ''
for _ in range(4):
    ab = ''
    for _ in range(5):
        ab += aa
        aa += z
if ab == '1':
    ac = ab + ' c1'
elif ab == '16':
    ac = ab + ' c2'
else:
    ac = ab + ' c3'
ad = (ac, ac, ac)
ae, af, ag = ad
ah = ae + af + ag
if ah == '7':
    ai = ah + ' c1'
elif ah == '19':
    ai = ah + ' c2'
else:
    ai = ah + ' c3'
print(ai)