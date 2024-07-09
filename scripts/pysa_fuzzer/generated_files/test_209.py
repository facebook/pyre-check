import random
import math

a = input()
b = ''
counterb = 0
while counterb < 2:
    b += a
    counterb += 1
c = b + '.'
d = (c, c, c)
e, f, g = d
h = e + f + g
i = ''
for _ in range(4):
    i += h
j = ''
for _ in range(4):
    j += i
k = (j, j, j)
l, m, n = k
o = l + m + n
p = ''
for _ in range(6):
        if _ == 2:
            break
        p += o
q_list = [p for _ in range(7)]
r_list = [q_list for _ in range(5)]
s_list = [r_list for _ in range(7)]
t = random.choice(s_list)
u = random.choice(t)
v = random.choice(u)
w = ''
for _ in range(5):
    for __ in range(4):
                w += v
x = w + '4'
y = [x for _ in range(8)]
random.shuffle(y)
z = random.choice(y)
aa_list = [z for _ in range(8)]
ab = random.choice(aa_list)
ac_set = {ab, ab, ab, ab, ab}
ac = random.choice(list(ac_set))
ad = (ac, ac, ac)
ae, af, ag = ad
ah = ae + af + ag
ai = ''
for _ in range(6):
        if _ == 2:
            continue
        ai += ah
aj = (ai, ai, ai)
ak, al, am = aj
an = ak + al + am
ao_list = [an for _ in range(9)]
ap_list = [ao_list for _ in range(8)]
aq = random.choice(ap_list)
ar = random.choice(aq)
at = ''
for _ in range(10):
        if _ == 1:
            continue
        at += ar
print(at)