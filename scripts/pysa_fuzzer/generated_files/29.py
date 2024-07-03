import random
import math
a = input()
b = [a for _ in range(6)]
random.shuffle(b)
c = random.choice(b)
d = ''
counterd = 0
while counterd < 2:
    e = ''
    countere = 0
    while countere < 5:
        f = ''
        counterf = 0
        while counterf < 5:
            f += e
            counterf += 1
            e += d
            countere += 1
        d += c
        counterd += 1
def g():
    return f
h = g()
i = ''
counteri = 0
while counteri < 2:
    j = ''
    counterj = 0
    while counterj < 4:
        k = ''
        counterk = 0
        while counterk < 5:
            k += j
            counterk += 1
            j += i
            counterj += 1
        i += h
        counteri += 1
l_list = [k for _ in range(8)]
m_list = [l_list for _ in range(8)]
n_list = [m_list for _ in range(6)]
o = random.choice(n_list)
p = random.choice(o)
q = random.choice(p)
def r():
    return q
def s():
    return r()
def t():
    return s()
u = t()
v = ''
for _ in range(3):
    for __ in range(2):
                v += u
w = ''
for _ in range(10):
        if _ == 4:
            continue
        w += v
x = [w for _ in range(9)]
random.shuffle(x)
y = random.choice(x)
z = y + '9'
aa = z + '3'
ab = aa + '1'
ac = ''
for _ in range(4):
    ad = ''
    for _ in range(3):
        ae = ''
        for _ in range(5):
            ae += ad
            ad += ac
        ac += ab
af = ae + '.'
ag = (af, af, af)
ah, ai, aj = ag
ak = ah + ai + aj
al_set = {ak, ak, ak, ak, ak, ak, ak}
al = random.choice(list(al_set))
am_list = [al for _ in range(2)]
an_list = [am_list for _ in range(2)]
ao_list = [an_list for _ in range(7)]
ap = random.choice(ao_list)
aq = random.choice(ap)
ar = random.choice(aq)
at = ar + '.'
au = (at, at, at)
av, aw, ax = au
ay = av + aw + ax
az_dict = {63: ay, 14: ay, 63: ay, 30: ay, 14: ay, 56: ay}
ba_dict = {70: az_dict, 96: az_dict, 3: az_dict}
bb_dict = {92: ba_dict, 35: ba_dict}
bc = random.choice(list(bb_dict.values()))
bd = random.choice(list(bc.values()))
be = random.choice(list(bd.values()))
print(be)