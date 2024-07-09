import random
import math

a = input()
b = ''
counterb = 0
while counterb < 5:
    c = ''
    counterc = 0
    while counterc < 2:
        c += b
        counterc += 1
        b += a
        counterb += 1
def d():
    return c
def e():
    return d()
def f():
    return e()
g = f()
h = [g for _ in range(8)]
random.shuffle(h)
i = random.choice(h)
def j():
    return i
def k():
    return j()
l = k()
m_set = {l, l, l}
m = random.choice(list(m_set))
n = f'string {m}'
o_set = {n, n, n, n, n, n}
o = random.choice(list(o_set))
p_list = [o for _ in range(6)]
q = random.choice(p_list)
if q == q:
    t = q + 'c1'
elif q == '19':
    t = r + 'c2'
else:
    t = s + 'c3'
u = f'string {t}'
v = u + '.'
w = f'string {v}'
x_list = [w for _ in range(10)]
y = random.choice(x_list)
z = y + '2'
aa = z + '3'
if aa == aa:
    ad = aa + 'c1'
elif aa == '12':
    ad = ab + 'c2'
else:
    ad = ac + 'c3'
ae = (ad, ad, ad)
af, ag, ah = ae
ai = af + ag + ah
aj = ''
for _ in range(2):
    for __ in range(2):
                aj += ai
ak_list = [aj for _ in range(6)]
al = random.choice(ak_list)
print(al)