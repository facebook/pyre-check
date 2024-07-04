import random
import math
a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g = ''
counterg = 0
while counterg < 3:
    h = ''
    counterh = 0
    while counterh < 2:
        h += g
        counterh += 1
        g += f
        counterg += 1
i = ''
for _ in range(2):
    for __ in range(5):
                i += h
j = i + '.'
k = j + '5'
l = k + '1'
m = l + '4'
n = f'string {m}'
o = ''
countero = 0
while countero < 2:
    p = ''
    counterp = 0
    while counterp < 5:
        p += o
        counterp += 1
        o += n
        countero += 1
q = p + '5'
r = q + '7'
s = r + '7'
t = f'string {s}'
u = ''
for _ in range(4):
    for __ in range(5):
                u += t
v = ''
for _ in range(4):
    for __ in range(2):
                v += u
def w():
    return v
def x():
    return w()
def y():
    return x()
z = y()
aa_set = {z, z, z, z, z}
aa = random.choice(list(aa_set))
ab = (aa, aa, aa)
ac, ad, ae = ab
af = ac + ad + ae
ag = ''
for _ in range(10):
        if _ == 3:
            continue
        ag += af
ah_list = [ag for _ in range(8)]
ai_list = [ah_list for _ in range(8)]
aj = random.choice(ai_list)
ak = random.choice(aj)
al_dict = {59: ak, 19: ak, 22: ak, 84: ak}
am = random.choice(list(al_dict.values()))
an = ''
for _ in range(7):
        if _ == 1:
            continue
        an += am
print(an)