import random
import math

a = input()
b_dict = {94: a, 51: a, 4: a, 85: a}
c = random.choice(list(b_dict.values()))
d_dict = {49: c, 95: c, 2: c}
e = random.choice(list(d_dict.values()))
f_set = {e, e, e, e, e, e}
f = random.choice(list(f_set))
g_set = {f, f, f}
g = random.choice(list(g_set))
h = ''
counterh = 0
while counterh < 4:
    i = ''
    counteri = 0
    while counteri < 2:
        i += h
        counteri += 1
        h += g
        counterh += 1
j = [i for _ in range(5)]
random.shuffle(j)
k = random.choice(j)
l = ''
counterl = 0
while counterl < 5:
    m = ''
    counterm = 0
    while counterm < 2:
        n = ''
        countern = 0
        while countern < 4:
            n += m
            countern += 1
            m += l
            counterm += 1
        l += k
        counterl += 1
o = n + '.'
p = (o, o, o)
q, r, s = p
t = q + r + s
u_list = [t for _ in range(2)]
v_list = [u_list for _ in range(10)]
w_list = [v_list for _ in range(6)]
x = random.choice(w_list)
y = random.choice(x)
z = random.choice(y)
def aa():
    return z
def ab():
    return aa()
ac = ab()
ad = ''
counterad = 0
while counterad < 5:
    ae = ''
    counterae = 0
    while counterae < 2:
        ae += ad
        counterae += 1
        ad += ac
        counterad += 1
def af():
    return ae
ag = af()
ah = (ag, ag, ag)
ai, aj, ak = ah
al = ai + aj + ak
am_set = {al, al}
am = random.choice(list(am_set))
if am == am:
    ap = am + 'c1'
elif am == '19':
    ap = an + 'c2'
else:
    ap = ao + 'c3'
aq = ''
for _ in range(4):
    ar = ''
    for _ in range(4):
        at = ''
        for _ in range(3):
            at += ar
            ar += aq
        aq += ap
au = at[0:]
print(au)