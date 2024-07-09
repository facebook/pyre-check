import random
import math

a = input()
b_dict = {52: a, 67: a, 33: a, 55: a, 42: a, 71: a, 26: a}
c_dict = {27: b_dict, 88: b_dict, 27: b_dict, 58: b_dict, 2: b_dict, 9: b_dict, 42: b_dict, 32: b_dict}
d_dict = {14: c_dict, 67: c_dict, 50: c_dict, 82: c_dict, 59: c_dict, 58: c_dict}
e = random.choice(list(d_dict.values()))
f = random.choice(list(e.values()))
g = random.choice(list(f.values()))
if g == g:
    j = g + 'c1'
elif g == '19':
    j = h + 'c2'
else:
    j = i + 'c3'
k = (j, j, j)
l, m, n = k
o = l + m + n
p = f'string {o}'
q = p[0:]
r = ''
counterr = 0
while counterr < 2:
    s = ''
    counters = 0
    while counters < 5:
        s += r
        counters += 1
        r += q
        counterr += 1
t = (s, s, s)
u, v, w = t
x = u + v + w
y = ''
countery = 0
while countery < 5:
    z = ''
    counterz = 0
    while counterz < 2:
        z += y
        counterz += 1
        y += x
        countery += 1
aa = z[0:]
ab = [aa for _ in range(9)]
random.shuffle(ab)
ac = random.choice(ab)
ad = ''
for _ in range(7):
        if _ == 4:
            break
        ad += ac
ae_dict = {83: ad, 22: ad, 98: ad, 96: ad, 61: ad, 7: ad, 46: ad}
af_dict = {83: ae_dict, 35: ae_dict, 95: ae_dict, 32: ae_dict, 38: ae_dict, 97: ae_dict, 69: ae_dict, 96: ae_dict, 79: ae_dict, 34: ae_dict}
ag_dict = {92: af_dict, 36: af_dict, 62: af_dict, 31: af_dict, 96: af_dict}
ah = random.choice(list(ag_dict.values()))
ai = random.choice(list(ah.values()))
aj = random.choice(list(ai.values()))
ak = aj[0:]
al_list = [ak for _ in range(7)]
am_list = [al_list for _ in range(10)]
an_list = [am_list for _ in range(9)]
ao = random.choice(an_list)
ap = random.choice(ao)
aq = random.choice(ap)
ar = ''
counterar = 0
while counterar < 4:
    at = ''
    counterat = 0
    while counterat < 5:
        au = ''
        counterau = 0
        while counterau < 4:
            au += at
            counterau += 1
            at += ar
            counterat += 1
        ar += aq
        counterar += 1
av = [au for _ in range(9)]
random.shuffle(av)
aw = random.choice(av)
ax = (aw, aw, aw)
ay, az, ba = ax
bb = ay + az + ba
bc = [bb for _ in range(5)]
random.shuffle(bc)
bd = random.choice(bc)
print(bd)