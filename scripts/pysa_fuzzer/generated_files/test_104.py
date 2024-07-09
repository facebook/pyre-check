import random
import math

a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g = ''
for _ in range(5):
    h = ''
    for _ in range(3):
        i = ''
        for _ in range(3):
            i += h
            h += g
        g += f
j_list = [i for _ in range(4)]
k = random.choice(j_list)
l = k[0:]
m_list = [l for _ in range(5)]
n_list = [m_list for _ in range(10)]
o_list = [n_list for _ in range(7)]
p = random.choice(o_list)
q = random.choice(p)
r = random.choice(q)
s_list = [r for _ in range(5)]
t_list = [s_list for _ in range(9)]
u = random.choice(t_list)
v = random.choice(u)
w = v[0:]
x = w + '6'
y = x + '1'
z = y + '6'
aa = z + '.'
ab = (aa, aa, aa)
ac, ad, ae = ab
af = ac + ad + ae
ag = af + '5'
ah = ag + '4'
if ah == ah:
    ak = ah + 'c1'
elif ah == '19':
    ak = ai + 'c2'
else:
    ak = aj + 'c3'
al = f'string {ak}'
am_list = [al for _ in range(5)]
an_list = [am_list for _ in range(2)]
ao_list = [an_list for _ in range(10)]
ap = random.choice(ao_list)
aq = random.choice(ap)
ar = random.choice(aq)
at = ''
for _ in range(5):
        if _ == 1:
            break
        at += ar
au = ''
for _ in range(5):
        if _ == 5:
            break
        au += at
av = ''
for _ in range(4):
    aw = ''
    for _ in range(4):
        aw += av
        av += au
ax = ''
for _ in range(5):
    ay = ''
    for _ in range(2):
        az = ''
        for _ in range(5):
            az += ay
            ay += ax
        ax += aw
print(az)