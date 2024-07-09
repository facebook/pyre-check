import random
import math

a = input()
b = ''
for _ in range(4):
    for __ in range(3):
                b += a
c = f'string {b}'
d = ''
for _ in range(2):
    e = ''
    for _ in range(3):
        f = ''
        for _ in range(5):
            f += e
            e += d
        d += c
def g():
    return f
def h():
    return g()
def i():
    return h()
j = i()
k = ''
for _ in range(6):
        if _ == 3:
            continue
        k += j
l = ''
for _ in range(3):
    l += k
m = [l for _ in range(7)]
random.shuffle(m)
n = random.choice(m)
o = ''
for _ in range(3):
    for __ in range(3):
                o += n
p = o + '.'
q = (p, p, p)
r, s, t = q
u = r + s + t
v = ''
for _ in range(2):
    for __ in range(2):
                v += u
w = v[0:]
x_dict = {65: w, 23: w, 27: w, 68: w}
y = random.choice(list(x_dict.values()))
z_list = [y for _ in range(3)]
aa_list = [z_list for _ in range(2)]
ab_list = [aa_list for _ in range(5)]
ac = random.choice(ab_list)
ad = random.choice(ac)
ae = random.choice(ad)
af_list = [ae for _ in range(2)]
ag_list = [af_list for _ in range(9)]
ah_list = [ag_list for _ in range(9)]
ai = random.choice(ah_list)
aj = random.choice(ai)
ak = random.choice(aj)
al = ak + '.'
am = al + '1'
an = am + '8'
ao = an + '1'
ap_list = [ao for _ in range(9)]
aq_list = [ap_list for _ in range(7)]
ar = random.choice(aq_list)
at = random.choice(ar)
print(at)