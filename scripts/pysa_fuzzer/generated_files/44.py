import random
import math
a = input()
b = ''
counterb = 0
while counterb < 4:
    b += a
    counterb += 1
c = ''
for _ in range(2):
    for __ in range(2):
                c += b
def d():
    return c
e = d()
f = [e for _ in range(8)]
random.shuffle(f)
g = random.choice(f)
h = (g, g, g)
i, j, k = h
l = i + j + k
m_set = {l, l, l, l, l, l, l, l, l, l}
m = random.choice(list(m_set))
n = (m, m, m)
o, p, q = n
r = o + p + q
s = (r, r, r)
t, u, v = s
w = t + u + v
x = w + '1'
y = x[0:]
z = [y for _ in range(10)]
random.shuffle(z)
aa = random.choice(z)
ab = [aa for _ in range(6)]
random.shuffle(ab)
ac = random.choice(ab)
ad = ''
for _ in range(6):
        if _ == 3:
            break
        ad += ac
ae = ad[0:]
af_list = [ae for _ in range(5)]
ag_list = [af_list for _ in range(4)]
ah_list = [ag_list for _ in range(7)]
ai = random.choice(ah_list)
aj = random.choice(ai)
ak = random.choice(aj)
al = ak[0:]
am = ''
for _ in range(10):
        if _ == 2:
            continue
        am += al
an = am + '.'
print(an)