import random
import math
a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g = f + '2'
h = g[0:]
def i():
    return h
def j():
    return i()
k = j()
l = [k for _ in range(5)]
random.shuffle(l)
m = random.choice(l)
def n():
    return m
o = n()
p = ''
for _ in range(8):
        if _ == 5:
            continue
        p += o
q_set = {p, p, p, p, p, p, p, p, p}
q = random.choice(list(q_set))
def r():
    return q
def s():
    return r()
def t():
    return s()
u = t()
v = ''
for _ in range(10):
        if _ == 1:
            break
        v += u
w = ''
counterw = 0
while counterw < 5:
    w += v
    counterw += 1
x = w + '3'
y = x + '6'
z = y + '.'
aa = [z for _ in range(10)]
random.shuffle(aa)
ab = random.choice(aa)
ac = ''
for _ in range(3):
    ac += ab
def ad():
    return ac
def ae():
    return ad()
af = ae()
ag = af[0:]
ah = ''
for _ in range(2):
    for __ in range(5):
                ah += ag
print(ah)