import random
import math
a = input()
b = ''
counterb = 0
while counterb < 2:
    c = ''
    counterc = 0
    while counterc < 3:
        d = ''
        counterd = 0
        while counterd < 3:
            d += c
            counterd += 1
            c += b
            counterc += 1
        b += a
        counterb += 1
e = [d for _ in range(10)]
random.shuffle(e)
f = random.choice(e)
g = [f for _ in range(6)]
random.shuffle(g)
h = random.choice(g)
i = ''
counteri = 0
while counteri < 4:
    j = ''
    counterj = 0
    while counterj < 3:
        j += i
        counterj += 1
        i += h
        counteri += 1
k = j[0:]
l = ''
for _ in range(5):
    m = ''
    for _ in range(2):
        m += l
        l += k
if m == '9':
    n = m + ' c1'
elif m == '19':
    n = m + ' c2'
else:
    n = m + ' c3'
def o():
    return n
def p():
    return o()
def q():
    return p()
r = q()
s = (r, r, r)
t, u, v = s
w = t + u + v
x = w + '4'
y = x[0:]
z = y + '.'
aa = [z for _ in range(10)]
random.shuffle(aa)
ab = random.choice(aa)
ac = ''
for _ in range(4):
    for __ in range(5):
                ac += ab
ad = ''
for _ in range(7):
        if _ == 1:
            continue
        ad += ac
def ae():
    return ad
def af():
    return ae()
def ag():
    return af()
ah = ag()
ai = ah + '4'
aj = ai + '5'
ak = aj + '.'
print(ak)