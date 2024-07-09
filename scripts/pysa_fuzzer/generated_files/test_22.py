import random
import math

a = input()
b = f'string {a}'
c = [b for _ in range(7)]
random.shuffle(c)
d = random.choice(c)
e_set = {d, d, d, d, d}
e = random.choice(list(e_set))
f = (e, e, e)
g, h, i = f
j = g + h + i
k_dict = {32: j, 82: j, 20: j, 90: j}
l = random.choice(list(k_dict.values()))
m = (l, l, l)
n, o, p = m
q = n + o + p
r = ''
for _ in range(5):
    for __ in range(4):
                r += q
s = ''
counters = 0
while counters < 2:
    t = ''
    countert = 0
    while countert < 3:
        u = ''
        counteru = 0
        while counteru < 2:
            u += t
            counteru += 1
            t += s
            countert += 1
        s += r
        counters += 1
v = u + '.'
w = (v, v, v)
x, y, z = w
aa = x + y + z
ab = ''
for _ in range(9):
        if _ == 1:
            break
        ab += aa
ac = [ab for _ in range(10)]
random.shuffle(ac)
ad = random.choice(ac)
ae_set = {ad, ad, ad, ad, ad, ad}
ae = random.choice(list(ae_set))
af = ae + '6'
ag = af + '3'
ah_list = [ag for _ in range(3)]
ai_list = [ah_list for _ in range(4)]
aj = random.choice(ai_list)
ak = random.choice(aj)
al = ak + '9'
am = al + '5'
an = f'string {am}'
ao = ''
for _ in range(5):
    ao += an
print(ao)