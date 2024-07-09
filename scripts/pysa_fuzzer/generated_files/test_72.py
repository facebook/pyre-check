import random
import math

a = input()
b = a + '4'
c = ''
for _ in range(6):
        if _ == 2:
            continue
        c += b
d = [c for _ in range(7)]
random.shuffle(d)
e = random.choice(d)
f = ''
for _ in range(5):
    f += e
g = f[0:]
h_dict = {72: g, 75: g, 73: g, 23: g, 95: g, 46: g}
i = random.choice(list(h_dict.values()))
j = ''
for _ in range(3):
    k = ''
    for _ in range(5):
        k += j
        j += i
if k == k:
    n = k + 'c1'
elif k == '17':
    n = l + 'c2'
else:
    n = m + 'c3'
o_list = [n for _ in range(7)]
p_list = [o_list for _ in range(10)]
q = random.choice(p_list)
r = random.choice(q)
s = ''
for _ in range(5):
        if _ == 4:
            break
        s += r
t = f'string {s}'
u_set = {t, t, t}
u = random.choice(list(u_set))
v = [u for _ in range(6)]
random.shuffle(v)
w = random.choice(v)
x = w[0:]
y = [x for _ in range(5)]
random.shuffle(y)
z = random.choice(y)
aa_set = {z, z, z, z, z, z, z, z}
aa = random.choice(list(aa_set))
ab_set = {aa, aa, aa, aa, aa, aa, aa, aa, aa, aa}
ab = random.choice(list(ab_set))
ac = ab[0:]
print(ac)