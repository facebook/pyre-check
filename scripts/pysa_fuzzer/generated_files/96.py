import random
import math
a = input()
b = ''
for _ in range(2):
    c = ''
    for _ in range(2):
        d = ''
        for _ in range(4):
            d += c
            c += b
        b += a
e = ''
for _ in range(4):
    for __ in range(3):
                e += d
f = [e for _ in range(10)]
random.shuffle(f)
g = random.choice(f)
def h():
    return g
def i():
    return h()
def j():
    return i()
k = j()
l = k + '7'
m = l + '7'
n = m + '4'
o = n[0:]
p = ''
for _ in range(4):
    for __ in range(3):
                p += o
def q():
    return p
def r():
    return q()
s = r()
t = s + '3'
u = t + '6'
v = u + '2'
w = f'string {v}'
x = w + '2'
y = x + '7'
z = y + '8'
aa = [z for _ in range(7)]
random.shuffle(aa)
ab = random.choice(aa)
ac = f'string {ab}'
ad = (ac, ac, ac)
ae, af, ag = ad
ah = ae + af + ag
ai = ''
for _ in range(2):
    for __ in range(4):
                ai += ah
aj = ''
for _ in range(5):
        if _ == 4:
            break
        aj += ai
if aj == '8':
    ak = aj + ' c1'
elif aj == '15':
    ak = aj + ' c2'
else:
    ak = aj + ' c3'
al = ak + '.'
print(al)