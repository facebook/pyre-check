import random
import math
a = input()
if a == '4':
    b = a + ' c1'
elif a == '13':
    b = a + ' c2'
else:
    b = a + ' c3'
c = ''
for _ in range(5):
    for __ in range(4):
                c += b
d = ''
for _ in range(2):
    e = ''
    for _ in range(2):
        f = ''
        for _ in range(4):
            f += e
            e += d
        d += c
g = [f for _ in range(6)]
random.shuffle(g)
h = random.choice(g)
i = (h, h, h)
j, k, l = i
m = j + k + l
n = [m for _ in range(7)]
random.shuffle(n)
o = random.choice(n)
if o == '7':
    p = o + ' c1'
elif o == '14':
    p = o + ' c2'
else:
    p = o + ' c3'
q = [p for _ in range(5)]
random.shuffle(q)
r = random.choice(q)
s = (r, r, r)
t, u, v = s
w = t + u + v
def x():
    return w
def y():
    return x()
def z():
    return y()
aa = z()
ab = aa[0:]
ac_set = {ab, ab, ab, ab, ab, ab, ab, ab, ab, ab}
ac = random.choice(list(ac_set))
ad = f'string {ac}'
ae = ''
for _ in range(4):
    for __ in range(5):
                ae += ad
af = ''
counteraf = 0
while counteraf < 5:
    ag = ''
    counterag = 0
    while counterag < 5:
        ah = ''
        counterah = 0
        while counterah < 4:
            ah += ag
            counterah += 1
            ag += af
            counterag += 1
        af += ae
        counteraf += 1
ai = [ah for _ in range(9)]
random.shuffle(ai)
aj = random.choice(ai)
ak = aj[0:]
al = ''
counteral = 0
while counteral < 3:
    am = ''
    counteram = 0
    while counteram < 2:
        an = ''
        counteran = 0
        while counteran < 3:
            an += am
            counteran += 1
            am += al
            counteram += 1
        al += ak
        counteral += 1
print(an)