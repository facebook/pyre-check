import random
import math

a = input()
b = a[0:]
c = b + '.'
d_list = [c for _ in range(5)]
e_list = [d_list for _ in range(6)]
f_list = [e_list for _ in range(10)]
g = random.choice(f_list)
h = random.choice(g)
i = random.choice(h)
j = i + '7'
k = [j for _ in range(6)]
random.shuffle(k)
l = random.choice(k)
m = [l for _ in range(7)]
random.shuffle(m)
n = random.choice(m)
o = n + '.'
p = ''
for _ in range(8):
        if _ == 5:
            continue
        p += o
q = ''
for _ in range(2):
    q += p
r = ''
for _ in range(3):
    for __ in range(2):
                r += q
if r == r:
    u = r + 'c1'
elif r == '17':
    u = s + 'c2'
else:
    u = t + 'c3'
v = u + '.'
w_set = {v, v}
w = random.choice(list(w_set))
x = ''
for _ in range(5):
    for __ in range(2):
                x += w
y = f'string {x}'
z = y + '.'
aa = z[0:]
ab = aa + '1'
ac = ab + '8'
ad = ac + '1'
print(ad)