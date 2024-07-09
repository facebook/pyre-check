import random
import math

a = input()
b = a + '5'
c_list = [b for _ in range(5)]
d_list = [c_list for _ in range(9)]
e_list = [d_list for _ in range(3)]
f = random.choice(e_list)
g = random.choice(f)
h = random.choice(g)
i_dict = {81: h, 41: h}
j = random.choice(list(i_dict.values()))
k = j + '1'
l = k + '4'
m = l + '6'
n = ''
for _ in range(5):
    for __ in range(4):
                n += m
o = n + '1'
p_list = [o for _ in range(3)]
q_list = [p_list for _ in range(4)]
r_list = [q_list for _ in range(10)]
s = random.choice(r_list)
t = random.choice(s)
u = random.choice(t)
v = ''
for _ in range(4):
    w = ''
    for _ in range(2):
        w += v
        v += u
x = w + '.'
y = (x, x, x)
z, aa, ab = y
ac = z + aa + ab
ad = f'string {ac}'
ae = ''
for _ in range(4):
    for __ in range(3):
                ae += ad
af = (ae, ae, ae)
ag, ah, ai = af
aj = ag + ah + ai
ak = (aj, aj, aj)
al, am, an = ak
ao = al + am + an
ap_list = [ao for _ in range(4)]
aq = random.choice(ap_list)
ar = (aq, aq, aq)
at, au, av = ar
aw = at + au + av
ax = ''
for _ in range(4):
    for __ in range(3):
                ax += aw
ay = [ax for _ in range(6)]
random.shuffle(ay)
az = random.choice(ay)
print(az)