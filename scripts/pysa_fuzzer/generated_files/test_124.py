import random
import math

a = input()
b = ''
for _ in range(7):
        if _ == 3:
            break
        b += a
c = f'string {b}'
d = (c, c, c)
e, f, g = d
h = e + f + g
i = (h, h, h)
j, k, l = i
m = j + k + l
n = ''
for _ in range(5):
    n += m
o = ''
for _ in range(2):
    p = ''
    for _ in range(4):
        p += o
        o += n
q = p + '7'
r = f'string {q}'
s = ''
for _ in range(4):
    for __ in range(4):
                s += r
t = ''
for _ in range(8):
        if _ == 5:
            break
        t += s
u = ''
for _ in range(6):
        if _ == 5:
            break
        u += t
v = u + '.'
w = ''
counterw = 0
while counterw < 3:
    x = ''
    counterx = 0
    while counterx < 2:
        x += w
        counterx += 1
        w += v
        counterw += 1
def y():
    return x
z = y()
aa = ''
counteraa = 0
while counteraa < 3:
    ab = ''
    counterab = 0
    while counterab < 5:
        ac = ''
        counterac = 0
        while counterac < 4:
            ac += ab
            counterac += 1
            ab += aa
            counterab += 1
        aa += z
        counteraa += 1
ad = ''
for _ in range(4):
    ae = ''
    for _ in range(2):
        ae += ad
        ad += ac
af_list = [ae for _ in range(6)]
ag_list = [af_list for _ in range(4)]
ah = random.choice(ag_list)
ai = random.choice(ah)
def aj():
    return ai
def ak():
    return aj()
al = ak()
print(al)