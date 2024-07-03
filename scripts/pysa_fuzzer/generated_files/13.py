import random
import math
a = input()
b = ''
for _ in range(6):
        if _ == 4:
            continue
        b += a
c = ''
for _ in range(3):
    c += b
d = c[0:]
e = f'string {d}'
def f():
    return e
def g():
    return f()
h = g()
i = ''
counteri = 0
while counteri < 3:
    i += h
    counteri += 1
j = (i, i, i)
k, l, m = j
n = k + l + m
o_dict = {76: n, 11: n, 78: n, 33: n, 22: n, 56: n, 35: n, 40: n, 74: n}
p = random.choice(list(o_dict.values()))
q_set = {p, p, p, p, p}
q = random.choice(list(q_set))
r_list = [q for _ in range(4)]
s_list = [r_list for _ in range(4)]
t_list = [s_list for _ in range(9)]
u = random.choice(t_list)
v = random.choice(u)
w = random.choice(v)
x = w[0:]
y = (x, x, x)
z, aa, ab = y
ac = z + aa + ab
ad = ''
for _ in range(5):
    for __ in range(4):
                ad += ac
ae = [ad for _ in range(9)]
random.shuffle(ae)
af = random.choice(ae)
ag = [af for _ in range(6)]
random.shuffle(ag)
ah = random.choice(ag)
ai = ah[0:]
aj = (ai, ai, ai)
ak, al, am = aj
an = ak + al + am
ao_list = [an for _ in range(7)]
ap_list = [ao_list for _ in range(4)]
aq_list = [ap_list for _ in range(2)]
ar = random.choice(aq_list)
at = random.choice(ar)
au = random.choice(at)
print(au)