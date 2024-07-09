import random
import math

a = input()
if a == a:
    d = a + 'c1'
elif a == '17':
    d = b + 'c2'
else:
    d = c + 'c3'
e = ''
for _ in range(8):
        if _ == 5:
            break
        e += d
f = ''
counterf = 0
while counterf < 4:
    g = ''
    counterg = 0
    while counterg < 5:
        g += f
        counterg += 1
        f += e
        counterf += 1
h_set = {g, g, g, g, g, g, g, g, g}
h = random.choice(list(h_set))
i = (h, h, h)
j, k, l = i
m = j + k + l
n = ''
for _ in range(3):
    n += m
o = [n for _ in range(8)]
random.shuffle(o)
p = random.choice(o)
q = p + '.'
r = ''
counterr = 0
while counterr < 2:
    s = ''
    counters = 0
    while counters < 2:
        s += r
        counters += 1
        r += q
        counterr += 1
t_list = [s for _ in range(6)]
u_list = [t_list for _ in range(7)]
v_list = [u_list for _ in range(6)]
w = random.choice(v_list)
x = random.choice(w)
y = random.choice(x)
z = ''
for _ in range(8):
        if _ == 4:
            continue
        z += y
aa = ''
for _ in range(2):
    for __ in range(2):
                aa += z
ab = ''
for _ in range(10):
        if _ == 4:
            break
        ab += aa
ac = ''
counterac = 0
while counterac < 4:
    ad = ''
    counterad = 0
    while counterad < 5:
        ad += ac
        counterad += 1
        ac += ab
        counterac += 1
ae = ''
counterae = 0
while counterae < 4:
    af = ''
    counteraf = 0
    while counteraf < 2:
        ag = ''
        counterag = 0
        while counterag < 2:
            ag += af
            counterag += 1
            af += ae
            counteraf += 1
        ae += ad
        counterae += 1
ah = ''
counterah = 0
while counterah < 2:
    ah += ag
    counterah += 1
ai = ''
for _ in range(2):
    for __ in range(2):
                ai += ah
aj = (ai, ai, ai)
ak, al, am = aj
an = ak + al + am
print(an)