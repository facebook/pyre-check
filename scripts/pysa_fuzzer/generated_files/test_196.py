import random
import math

a = input()
b_dict = {6: a, 87: a}
c_dict = {83: b_dict, 48: b_dict, 32: b_dict}
d_dict = {11: c_dict, 91: c_dict, 5: c_dict}
e = random.choice(list(d_dict.values()))
f = random.choice(list(e.values()))
g = random.choice(list(f.values()))
h_set = {g, g, g, g, g, g, g}
h = random.choice(list(h_set))
i = (h, h, h)
j, k, l = i
m = j + k + l
n = ''
for _ in range(5):
    for __ in range(3):
                n += m
o = n[0:]
p = ''
counterp = 0
while counterp < 5:
    q = ''
    counterq = 0
    while counterq < 2:
        r = ''
        counterr = 0
        while counterr < 2:
            r += q
            counterr += 1
            q += p
            counterq += 1
        p += o
        counterp += 1
def s():
    return r
t = s()
u = (t, t, t)
v, w, x = u
y = v + w + x
if y == y:
    ab = y + 'c1'
elif y == '12':
    ab = z + 'c2'
else:
    ab = aa + 'c3'
ac = [ab for _ in range(5)]
random.shuffle(ac)
ad = random.choice(ac)
ae = ad[0:]
af = [ae for _ in range(9)]
random.shuffle(af)
ag = random.choice(af)
ah_list = [ag for _ in range(10)]
ai = random.choice(ah_list)
aj = (ai, ai, ai)
ak, al, am = aj
an = ak + al + am
ao = ''
for _ in range(4):
    ao += an
ap = ''
for _ in range(6):
        if _ == 2:
            break
        ap += ao
aq_list = [ap for _ in range(4)]
ar_list = [aq_list for _ in range(7)]
at = random.choice(ar_list)
au = random.choice(at)
av = au[0:]
print(av)