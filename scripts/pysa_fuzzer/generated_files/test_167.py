import random
import math

a = input()
b = a + '6'
c = (b, b, b)
d, e, f = c
g = d + e + f
h = f'string {g}'
i = h + '.'
j = i + '.'
k_set = {j, j}
k = random.choice(list(k_set))
l_list = [k for _ in range(9)]
m_list = [l_list for _ in range(10)]
n = random.choice(m_list)
o = random.choice(n)
p = o + '.'
q = p + '4'
r = q + '9'
s = r + '3'
t_set = {s, s, s, s}
t = random.choice(list(t_set))
u_set = {t, t, t, t, t, t, t, t, t, t}
u = random.choice(list(u_set))
v = u + '.'
w = ''
for _ in range(10):
        if _ == 1:
            continue
        w += v
x = [w for _ in range(6)]
random.shuffle(x)
y = random.choice(x)
z = f'string {y}'
aa = z + '8'
ab = (aa, aa, aa)
ac, ad, ae = ab
af = ac + ad + ae
ag = af + '8'
ah = ag + '4'
print(ah)