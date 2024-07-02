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
    while counterh < 4:
        h += g
        counterh += 1
        g += f
        counterg += 1
i = f'string {h}'
j = ''
counterj = 0
while counterj < 3:
    k = ''
    counterk = 0
    while counterk < 2:
        l = ''
        counterl = 0
        while counterl < 2:
            l += k
            counterl += 1
            k += j
            counterk += 1
        j += i
        counterj += 1
m = l + '.'
n = m[0:]
o = n + '7'
p = o + '6'
q = ''
for _ in range(5):
    for __ in range(4):
                q += p
r = ''
counterr = 0
while counterr < 4:
    s = ''
    counters = 0
    while counters < 3:
        s += r
        counters += 1
        r += q
        counterr += 1
def t():
    return s
def u():
    return t()
def v():
    return u()
w = v()
x = ''
for _ in range(5):
        if _ == 4:
            continue
        x += w
if x == '5':
    y = x + ' c1'
elif x == '19':
    y = x + ' c2'
else:
    y = x + ' c3'
z = f'string {y}'
aa_set = {z, z, z, z, z}
aa = random.choice(list(aa_set))
ab_set = {aa, aa}
ab = random.choice(list(ab_set))
if ab == '6':
    ac = ab + ' c1'
elif ab == '16':
    ac = ab + ' c2'
else:
    ac = ab + ' c3'
ad = ac[0:]
ae_dict = {41: ad, 37: ad, 38: ad, 55: ad}
af_dict = {74: ae_dict, 16: ae_dict, 84: ae_dict, 42: ae_dict, 14: ae_dict, 10: ae_dict, 80: ae_dict, 69: ae_dict, 77: ae_dict, 95: ae_dict}
ag = random.choice(list(af_dict.values()))
ah = random.choice(list(ag.values()))
print(ah)