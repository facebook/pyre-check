import random
import math

a = input()
b_set = {a, a, a, a, a, a}
b = random.choice(list(b_set))
def c():
    return b
d = c()
e = ''
for _ in range(4):
    for __ in range(3):
                e += d
f_dict = {48: e, 66: e, 29: e, 15: e, 52: e, 54: e}
g = random.choice(list(f_dict.values()))
h = ''
counterh = 0
while counterh < 5:
    h += g
    counterh += 1
i = [h for _ in range(5)]
random.shuffle(i)
j = random.choice(i)
k = j[0:]
l_set = {k, k, k, k, k, k, k, k, k, k}
l = random.choice(list(l_set))
m = (l, l, l)
n, o, p = m
q = n + o + p
r = (q, q, q)
s, t, u = r
v = s + t + u
w = [v for _ in range(6)]
random.shuffle(w)
x = random.choice(w)
y = ''
for _ in range(5):
        if _ == 4:
            continue
        y += x
z = (y, y, y)
aa, ab, ac = z
ad = aa + ab + ac
ae_list = [ad for _ in range(3)]
af_list = [ae_list for _ in range(9)]
ag_list = [af_list for _ in range(4)]
ah = random.choice(ag_list)
ai = random.choice(ah)
aj = random.choice(ai)
ak_list = [aj for _ in range(6)]
al = random.choice(ak_list)
am = al + '.'
an = (am, am, am)
ao, ap, aq = an
ar = ao + ap + aq
at = ''
for _ in range(5):
    au = ''
    for _ in range(3):
        au += at
        at += ar
print(au)