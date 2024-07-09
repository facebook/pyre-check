import random
import math

a = input()
if a == a:
    d = a + 'c1'
elif a == '12':
    d = b + 'c2'
else:
    d = c + 'c3'
e = d + '1'
f = e + '1'
g = f + '9'
h = g + '.'
i = ''
for _ in range(6):
        if _ == 5:
            break
        i += h
j = i + '6'
k = j + '.'
l = ''
for _ in range(5):
        if _ == 1:
            break
        l += k
m_set = {l, l, l, l}
m = random.choice(list(m_set))
n = [m for _ in range(9)]
random.shuffle(n)
o = random.choice(n)
p_list = [o for _ in range(10)]
q_list = [p_list for _ in range(6)]
r = random.choice(q_list)
s = random.choice(r)
t_set = {s, s, s, s, s, s, s, s}
t = random.choice(list(t_set))
u = t[0:]
v_set = {u, u, u, u, u, u}
v = random.choice(list(v_set))
w = ''
for _ in range(5):
    x = ''
    for _ in range(4):
        y = ''
        for _ in range(3):
            y += x
            x += w
        w += v
z = (y, y, y)
aa, ab, ac = z
ad = aa + ab + ac
ae_set = {ad, ad, ad, ad, ad}
ae = random.choice(list(ae_set))
af_set = {ae, ae, ae, ae, ae, ae, ae}
af = random.choice(list(af_set))
ag = ''
for _ in range(8):
        if _ == 3:
            continue
        ag += af
print(ag)