import random
import math

a = input()
b_set = {a, a, a, a, a, a, a, a}
b = random.choice(list(b_set))
c = f'string {b}'
d_set = {c, c, c, c, c}
d = random.choice(list(d_set))
e = [d for _ in range(5)]
random.shuffle(e)
f = random.choice(e)
g = f + '7'
h_list = [g for _ in range(8)]
i_list = [h_list for _ in range(7)]
j_list = [i_list for _ in range(9)]
k = random.choice(j_list)
l = random.choice(k)
m = random.choice(l)
n = ''
countern = 0
while countern < 3:
    o = ''
    countero = 0
    while countero < 5:
        o += n
        countero += 1
        n += m
        countern += 1
p = [o for _ in range(9)]
random.shuffle(p)
q = random.choice(p)
r_set = {q, q}
r = random.choice(list(r_set))
s = ''
for _ in range(5):
    for __ in range(3):
                s += r
t = (s, s, s)
u, v, w = t
x = u + v + w
y = x + '1'
z = ''
for _ in range(5):
    aa = ''
    for _ in range(5):
        ab = ''
        for _ in range(3):
            ab += aa
            aa += z
        z += y
if ab == ab:
    ae = ab + 'c1'
elif ab == '18':
    ae = ac + 'c2'
else:
    ae = ad + 'c3'
af = (ae, ae, ae)
ag, ah, ai = af
aj = ag + ah + ai
ak = ''
for _ in range(2):
    for __ in range(2):
                ak += aj
al = ''
counteral = 0
while counteral < 2:
    al += ak
    counteral += 1
am_list = [al for _ in range(8)]
an_list = [am_list for _ in range(5)]
ao_list = [an_list for _ in range(5)]
ap = random.choice(ao_list)
aq = random.choice(ap)
ar = random.choice(aq)
print(ar)