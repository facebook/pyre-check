import random
import math

a = input()
b_list = [a for _ in range(4)]
c_list = [b_list for _ in range(3)]
d_list = [c_list for _ in range(7)]
e = random.choice(d_list)
f = random.choice(e)
g = random.choice(f)
h = ''
for _ in range(8):
        if _ == 5:
            continue
        h += g
i_dict = {91: h, 9: h, 96: h, 85: h, 7: h, 33: h, 68: h, 72: h, 75: h}
j = random.choice(list(i_dict.values()))
k = ''
for _ in range(8):
        if _ == 4:
            continue
        k += j
l = k[0:]
m = [l for _ in range(10)]
random.shuffle(m)
n = random.choice(m)
o = ''
for _ in range(4):
    o += n
p = [o for _ in range(8)]
random.shuffle(p)
q = random.choice(p)
r_set = {q, q, q, q}
r = random.choice(list(r_set))
s = ''
for _ in range(4):
    t = ''
    for _ in range(5):
        u = ''
        for _ in range(4):
            u += t
            t += s
        s += r
v = u + '.'
def w():
    return v
x = w()
y = [x for _ in range(10)]
random.shuffle(y)
z = random.choice(y)
aa = f'string {z}'
if aa == aa:
    ad = aa + 'c1'
elif aa == '14':
    ad = ab + 'c2'
else:
    ad = ac + 'c3'
ae_list = [ad for _ in range(7)]
af_list = [ae_list for _ in range(10)]
ag_list = [af_list for _ in range(5)]
ah = random.choice(ag_list)
ai = random.choice(ah)
aj = random.choice(ai)
ak_set = {aj, aj, aj, aj}
ak = random.choice(list(ak_set))
al = (ak, ak, ak)
am, an, ao = al
ap = am + an + ao
print(ap)