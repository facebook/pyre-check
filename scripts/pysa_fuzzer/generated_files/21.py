import random
import math
a = input()
b_set = {a, a, a, a, a, a, a}
b = random.choice(list(b_set))
c_list = [b for _ in range(9)]
d_list = [c_list for _ in range(2)]
e_list = [d_list for _ in range(8)]
f = random.choice(e_list)
g = random.choice(f)
h = random.choice(g)
if h == '5':
    i = h + ' c1'
elif h == '12':
    i = h + ' c2'
else:
    i = h + ' c3'
j = f'string {i}'
if j == '9':
    k = j + ' c1'
elif j == '12':
    k = j + ' c2'
else:
    k = j + ' c3'
l_set = {k, k, k, k, k}
l = random.choice(list(l_set))
m = ''
for _ in range(4):
    m += l
n = ''
for _ in range(2):
    o = ''
    for _ in range(2):
        p = ''
        for _ in range(5):
            p += o
            o += n
        n += m
q_list = [p for _ in range(10)]
r_list = [q_list for _ in range(9)]
s_list = [r_list for _ in range(3)]
t = random.choice(s_list)
u = random.choice(t)
v = random.choice(u)
w = [v for _ in range(6)]
random.shuffle(w)
x = random.choice(w)
y = [x for _ in range(5)]
random.shuffle(y)
z = random.choice(y)
aa = [z for _ in range(9)]
random.shuffle(aa)
ab = random.choice(aa)
ac = ab + '.'
ad = f'string {ac}'
ae = ad + '6'
af = ae + '2'
ag = af + '4'
ah = [ag for _ in range(6)]
random.shuffle(ah)
ai = random.choice(ah)
aj = ai + '7'
ak = aj + '4'
al = ak + '8'
am_list = [al for _ in range(10)]
an_list = [am_list for _ in range(3)]
ao = random.choice(an_list)
ap = random.choice(ao)
print(ap)