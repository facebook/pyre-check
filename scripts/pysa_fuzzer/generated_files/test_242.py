import random
import math

a = input()
b = a + '.'
def c():
    return b
def d():
    return c()
def e():
    return d()
f = e()
g = f + '3'
h = f'string {g}'
i = ''
for _ in range(10):
        if _ == 4:
            continue
        i += h
j = ''
for _ in range(5):
    k = ''
    for _ in range(5):
        k += j
        j += i
l = ''
counterl = 0
while counterl < 3:
    m = ''
    counterm = 0
    while counterm < 5:
        n = ''
        countern = 0
        while countern < 3:
            n += m
            countern += 1
            m += l
            counterm += 1
        l += k
        counterl += 1
o = [n for _ in range(7)]
random.shuffle(o)
p = random.choice(o)
q_list = [p for _ in range(8)]
r = random.choice(q_list)
s = ''
for _ in range(4):
    s += r
t_set = {s, s, s, s, s, s, s, s, s, s}
t = random.choice(list(t_set))
u = ''
for _ in range(3):
    u += t
def v():
    return u
w = v()
x_list = [w for _ in range(9)]
y_list = [x_list for _ in range(10)]
z_list = [y_list for _ in range(9)]
aa = random.choice(z_list)
ab = random.choice(aa)
ac = random.choice(ab)
ad = ''
for _ in range(3):
    ae = ''
    for _ in range(5):
        ae += ad
        ad += ac
af_list = [ae for _ in range(6)]
ag_list = [af_list for _ in range(5)]
ah_list = [ag_list for _ in range(6)]
ai = random.choice(ah_list)
aj = random.choice(ai)
ak = random.choice(aj)
al_list = [ak for _ in range(7)]
am_list = [al_list for _ in range(2)]
an_list = [am_list for _ in range(5)]
ao = random.choice(an_list)
ap = random.choice(ao)
aq = random.choice(ap)
ar = ''
counterar = 0
while counterar < 5:
    at = ''
    counterat = 0
    while counterat < 3:
        at += ar
        counterat += 1
        ar += aq
        counterar += 1
print(at)