import random
import math
a = input()
b = a[0:]
def c():
    return b
def d():
    return c()
e = d()
f_set = {e, e, e, e, e}
f = random.choice(list(f_set))
g = (f, f, f)
h, i, j = g
k = h + i + j
l = ''
for _ in range(6):
        if _ == 4:
            continue
        l += k
m = l + '1'
n = m + '7'
o = n + '4'
p = o + '3'
q = ''
for _ in range(9):
        if _ == 2:
            break
        q += p
def r():
    return q
def s():
    return r()
t = s()
u = ''
for _ in range(3):
    u += t
v = f'string {u}'
w_set = {v, v, v, v}
w = random.choice(list(w_set))
x = [w for _ in range(10)]
random.shuffle(x)
y = random.choice(x)
def z():
    return y
def aa():
    return z()
ab = aa()
ac = ''
counterac = 0
while counterac < 5:
    ad = ''
    counterad = 0
    while counterad < 5:
        ae = ''
        counterae = 0
        while counterae < 3:
            ae += ad
            counterae += 1
            ad += ac
            counterad += 1
        ac += ab
        counterac += 1
def af():
    return ae
def ag():
    return af()
ah = ag()
ai = ah[0:]
def aj():
    return ai
def ak():
    return aj()
def al():
    return ak()
am = al()
print(am)