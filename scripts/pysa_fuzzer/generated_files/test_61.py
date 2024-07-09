import random
import math

a = input()
def b():
    return a
def c():
    return b()
def d():
    return c()
e = d()
f = ''
counterf = 0
while counterf < 2:
    g = ''
    counterg = 0
    while counterg < 2:
        g += f
        counterg += 1
        f += e
        counterf += 1
h_list = [g for _ in range(7)]
i_list = [h_list for _ in range(2)]
j = random.choice(i_list)
k = random.choice(j)
l_set = {k, k, k, k, k}
l = random.choice(list(l_set))
m = l + '.'
n = m[0:]
o_dict = {82: n, 75: n, 100: n, 89: n, 96: n, 79: n, 53: n, 64: n}
p = random.choice(list(o_dict.values()))
q = ''
counterq = 0
while counterq < 2:
    r = ''
    counterr = 0
    while counterr < 3:
        s = ''
        counters = 0
        while counters < 5:
            s += r
            counters += 1
            r += q
            counterr += 1
        q += p
        counterq += 1
t = ''
countert = 0
while countert < 3:
    u = ''
    counteru = 0
    while counteru < 4:
        v = ''
        counterv = 0
        while counterv < 2:
            v += u
            counterv += 1
            u += t
            counteru += 1
        t += s
        countert += 1
w = ''
for _ in range(3):
    w += v
x_list = [w for _ in range(3)]
y_list = [x_list for _ in range(6)]
z_list = [y_list for _ in range(7)]
aa = random.choice(z_list)
ab = random.choice(aa)
ac = random.choice(ab)
ad = ''
for _ in range(3):
    ae = ''
    for _ in range(4):
        ae += ad
        ad += ac
af = ''
counteraf = 0
while counteraf < 3:
    af += ae
    counteraf += 1
ag = ''
counterag = 0
while counterag < 3:
    ah = ''
    counterah = 0
    while counterah < 2:
        ai = ''
        counterai = 0
        while counterai < 3:
            ai += ah
            counterai += 1
            ah += ag
            counterah += 1
        ag += af
        counterag += 1
aj_dict = {30: ai, 76: ai, 91: ai, 91: ai, 74: ai, 24: ai, 47: ai, 48: ai}
ak_dict = {40: aj_dict, 82: aj_dict, 26: aj_dict, 65: aj_dict, 44: aj_dict, 23: aj_dict, 9: aj_dict, 31: aj_dict}
al = random.choice(list(ak_dict.values()))
am = random.choice(list(al.values()))
def an():
    return am
def ao():
    return an()
def ap():
    return ao()
aq = ap()
ar = ''
counterar = 0
while counterar < 2:
    ar += aq
    counterar += 1
at = ''
for _ in range(4):
    for __ in range(3):
                at += ar
print(at)