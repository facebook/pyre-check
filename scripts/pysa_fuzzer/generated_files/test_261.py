import random
import math

a = input()
b = ''
for _ in range(8):
        if _ == 4:
            continue
        b += a
c = f'string {b}'
d = ''
for _ in range(4):
    e = ''
    for _ in range(5):
        e += d
        d += c
f = e + '9'
g = f + '2'
h = ''
counterh = 0
while counterh < 2:
    i = ''
    counteri = 0
    while counteri < 4:
        j = ''
        counterj = 0
        while counterj < 2:
            j += i
            counterj += 1
            i += h
            counteri += 1
        h += g
        counterh += 1
k_dict = {8: j, 45: j, 96: j, 60: j, 86: j}
l = random.choice(list(k_dict.values()))
m = (l, l, l)
n, o, p = m
q = n + o + p
r = f'string {q}'
s_set = {r, r, r, r, r, r, r, r, r}
s = random.choice(list(s_set))
t = s[0:]
u_list = [t for _ in range(8)]
v_list = [u_list for _ in range(10)]
w_list = [v_list for _ in range(8)]
x = random.choice(w_list)
y = random.choice(x)
z = random.choice(y)
aa = f'string {z}'
ab = ''
for _ in range(5):
    ac = ''
    for _ in range(3):
        ad = ''
        for _ in range(2):
            ad += ac
            ac += ab
        ab += aa
ae = f'string {ad}'
af = ''
counteraf = 0
while counteraf < 3:
    ag = ''
    counterag = 0
    while counterag < 3:
        ag += af
        counterag += 1
        af += ae
        counteraf += 1
ah_list = [ag for _ in range(7)]
ai = random.choice(ah_list)
aj_list = [ai for _ in range(8)]
ak_list = [aj_list for _ in range(3)]
al_list = [ak_list for _ in range(8)]
am = random.choice(al_list)
an = random.choice(am)
ao = random.choice(an)
ap = ''
for _ in range(2):
    aq = ''
    for _ in range(3):
        ar = ''
        for _ in range(5):
            ar += aq
            aq += ap
        ap += ao
print(ar)