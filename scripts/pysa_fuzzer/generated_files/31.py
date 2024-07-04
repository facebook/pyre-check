import random
import math
a = input()
b = ''
for _ in range(2):
    c = ''
    for _ in range(5):
        d = ''
        for _ in range(4):
            d += c
            c += b
        b += a
e = d + '3'
f = ''
for _ in range(4):
    g = ''
    for _ in range(3):
        h = ''
        for _ in range(4):
            h += g
            g += f
        f += e
if h == '10':
    i = h + ' c1'
elif h == '19':
    i = h + ' c2'
else:
    i = h + ' c3'
j = [i for _ in range(6)]
random.shuffle(j)
k = random.choice(j)
l = ''
counterl = 0
while counterl < 3:
    l += k
    counterl += 1
m = ''
counterm = 0
while counterm < 3:
    m += l
    counterm += 1
n = ''
countern = 0
while countern < 2:
    o = ''
    countero = 0
    while countero < 4:
        p = ''
        counterp = 0
        while counterp < 2:
            p += o
            counterp += 1
            o += n
            countero += 1
        n += m
        countern += 1
q = (p, p, p)
r, s, t = q
u = r + s + t
v = ''
for _ in range(2):
    for __ in range(5):
                v += u
w = ''
for _ in range(4):
    w += v
x = (w, w, w)
y, z, aa = x
ab = y + z + aa
ac_set = {ab, ab, ab}
ac = random.choice(list(ac_set))
ad = ''
for _ in range(4):
    for __ in range(2):
                ad += ac
ae_set = {ad, ad, ad, ad, ad, ad, ad, ad, ad, ad}
ae = random.choice(list(ae_set))
af_dict = {68: ae, 49: ae, 33: ae, 5: ae, 34: ae, 93: ae, 91: ae, 88: ae}
ag_dict = {46: af_dict, 23: af_dict, 7: af_dict}
ah = random.choice(list(ag_dict.values()))
ai = random.choice(list(ah.values()))
aj = ''
counteraj = 0
while counteraj < 3:
    aj += ai
    counteraj += 1
ak = aj[0:]
print(ak)