import random
import math

a = input()
b_set = {a, a, a, a, a, a, a, a, a, a}
b = random.choice(list(b_set))
c = [b for _ in range(8)]
random.shuffle(c)
d = random.choice(c)
e_list = [d for _ in range(8)]
f_list = [e_list for _ in range(4)]
g_list = [f_list for _ in range(8)]
h = random.choice(g_list)
i = random.choice(h)
j = random.choice(i)
k = (j, j, j)
l, m, n = k
o = l + m + n
p_set = {o, o, o, o}
p = random.choice(list(p_set))
q = ''
for _ in range(9):
        if _ == 4:
            continue
        q += p
r = q + '.'
s = ''
for _ in range(10):
        if _ == 5:
            continue
        s += r
t = (s, s, s)
u, v, w = t
x = u + v + w
y_list = [x for _ in range(4)]
z_list = [y_list for _ in range(6)]
aa_list = [z_list for _ in range(4)]
ab = random.choice(aa_list)
ac = random.choice(ab)
ad = random.choice(ac)
ae = ad + '8'
af = ae + '7'
ag = af + '1'
ah = [ag for _ in range(6)]
random.shuffle(ah)
ai = random.choice(ah)
aj = (ai, ai, ai)
ak, al, am = aj
an = ak + al + am
ao = an + '5'
ap = ao + '7'
aq = ap + '7'
ar_list = [aq for _ in range(4)]
at = random.choice(ar_list)
au = ''
for _ in range(3):
    av = ''
    for _ in range(3):
        av += au
        au += at
aw = av[0:]
ax = ''
counterax = 0
while counterax < 3:
    ay = ''
    counteray = 0
    while counteray < 4:
        ay += ax
        counteray += 1
        ax += aw
        counterax += 1
print(ay)