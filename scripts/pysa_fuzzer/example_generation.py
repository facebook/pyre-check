import random
import math
a = input()
b = ''
for _ in range(5):
    for __ in range(2):
                b += a
c = ''
for _ in range(3):
    c += b
d = ''
counter = 0
while counter < 2:
    d += c
    counter += 1
e = f'string {d}'
f = ''
counter = 0
while counter < 3:
    f += e
    counter += 1
g = [f for _ in range(5)]
random.shuffle(g)
h = random.choice(g)
i = ''
for _ in range(2):
    i += h
j = ''
for _ in range(7):
        if _ == 5:
            continue
        j += i
k = ''
counter = 0
while counter < 2:
    k += j
    counter += 1
l = ''
for _ in range(4):
    l += k
m = ''
for _ in range(2):
    m += l
n_set = {m, m, m}
n = random.choice(list(n_set))
o_dict = {61: n, 29: n, 59: n, 29: n}
o = random.choice(list(o_dict.values()))
p = o[0:]
q = p[0:]
r = ''
for _ in range(4):
    for __ in range(2):
                r += q
s = ''
for _ in range(9):
        if _ == 3:
            continue
        s += r
if s == '3':
    t = s + ' c1'
elif s == '20':
    t = s + ' c2'
else:
    t = s + ' c3'
u = ''
for _ in range(2):
    u += t
v_dict = {58: u, 18: u}
v = random.choice(list(v_dict.values()))
w = v + '.'
x = w[0:]
y = [x for _ in range(7)]
random.shuffle(y)
z = random.choice(y)
aa = z + '.'
ab_set = {aa, aa, aa, aa}
ab = random.choice(list(ab_set))
ac = ''
for _ in range(5):
    ac += ab
ad = f'string {ac}'
ae_dict = {73: ad, 2: ad, 29: ad, 24: ad}
ae = random.choice(list(ae_dict.values()))
af = (ae, ae, ae)
ag, ah, ai = af
aj = ag + ah + ai
ak = aj + '.'
al = ak + '.'
am_set = {al, al, al, al, al, al, al}
am = random.choice(list(am_set))
if am == '1':
    an = am + ' c1'
elif am == '15':
    an = am + ' c2'
else:
    an = am + ' c3'
print(an)