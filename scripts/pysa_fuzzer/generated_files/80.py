import random
import math
a = input()
def b():
    return a
def c():
    return b()
d = c()
e = [d for _ in range(7)]
random.shuffle(e)
f = random.choice(e)
g = (f, f, f)
h, i, j = g
k = h + i + j
l = ''
counterl = 0
while counterl < 5:
    m = ''
    counterm = 0
    while counterm < 2:
        m += l
        counterm += 1
        l += k
        counterl += 1
n = ''
countern = 0
while countern < 4:
    n += m
    countern += 1
o = n + '.'
def p():
    return o
def q():
    return p()
r = q()
s = ''
for _ in range(4):
    t = ''
    for _ in range(4):
        u = ''
        for _ in range(2):
            u += t
            t += s
        s += r
v = [u for _ in range(9)]
random.shuffle(v)
w = random.choice(v)
x = (w, w, w)
y, z, aa = x
ab = y + z + aa
ac = ''
for _ in range(4):
    for __ in range(3):
                ac += ab
ad = ''
for _ in range(5):
    for __ in range(3):
                ad += ac
ae = ad[0:]
af = ae[0:]
ag = af[0:]
if ag == '7':
    ah = ag + ' c1'
elif ag == '16':
    ah = ag + ' c2'
else:
    ah = ag + ' c3'
ai = ''
counterai = 0
while counterai < 4:
    aj = ''
    counteraj = 0
    while counteraj < 5:
        ak = ''
        counterak = 0
        while counterak < 4:
            ak += aj
            counterak += 1
            aj += ai
            counteraj += 1
        ai += ah
        counterai += 1
al = [ak for _ in range(5)]
random.shuffle(al)
am = random.choice(al)
print(am)