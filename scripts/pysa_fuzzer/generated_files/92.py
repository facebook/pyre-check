import random
import math
a = input()
b = ''
for _ in range(3):
    c = ''
    for _ in range(4):
        d = ''
        for _ in range(2):
            d += c
            c += b
        b += a
e = [d for _ in range(5)]
random.shuffle(e)
f = random.choice(e)
g_list = [f for _ in range(10)]
h_list = [g_list for _ in range(4)]
i_list = [h_list for _ in range(3)]
j = random.choice(i_list)
k = random.choice(j)
l = random.choice(k)
m_list = [l for _ in range(5)]
n_list = [m_list for _ in range(8)]
o_list = [n_list for _ in range(9)]
p = random.choice(o_list)
q = random.choice(p)
r = random.choice(q)
s = r[0:]
t = ''
for _ in range(5):
        if _ == 5:
            break
        t += s
u = (t, t, t)
v, w, x = u
y = v + w + x
z = ''
for _ in range(4):
    aa = ''
    for _ in range(3):
        ab = ''
        for _ in range(5):
            ab += aa
            aa += z
        z += y
ac = ab[0:]
ad = f'string {ac}'
ae = ad[0:]
if ae == '6':
    af = ae + ' c1'
elif ae == '12':
    af = ae + ' c2'
else:
    af = ae + ' c3'
ag_list = [af for _ in range(10)]
ah_list = [ag_list for _ in range(2)]
ai = random.choice(ah_list)
aj = random.choice(ai)
ak = aj[0:]
if ak == '5':
    al = ak + ' c1'
elif ak == '17':
    al = ak + ' c2'
else:
    al = ak + ' c3'
am_list = [al for _ in range(5)]
an = random.choice(am_list)
ao = ''
counterao = 0
while counterao < 4:
    ap = ''
    counterap = 0
    while counterap < 3:
        ap += ao
        counterap += 1
        ao += an
        counterao += 1
aq_list = [ap for _ in range(8)]
ar = random.choice(aq_list)
print(ar)