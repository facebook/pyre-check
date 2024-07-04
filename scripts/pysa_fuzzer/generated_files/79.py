import random
import math
a = input()
b = ''
for _ in range(2):
    for __ in range(3):
                b += a
c_set = {b, b, b, b, b, b, b, b, b, b}
c = random.choice(list(c_set))
if c == '10':
    d = c + ' c1'
elif c == '11':
    d = c + ' c2'
else:
    d = c + ' c3'
e = ''
for _ in range(8):
        if _ == 1:
            continue
        e += d
f = e[0:]
g = ''
for _ in range(7):
        if _ == 1:
            break
        g += f
h_set = {g, g, g, g, g, g}
h = random.choice(list(h_set))
i_list = [h for _ in range(7)]
j_list = [i_list for _ in range(7)]
k = random.choice(j_list)
l = random.choice(k)
m = l[0:]
if m == '1':
    n = m + ' c1'
elif m == '12':
    n = m + ' c2'
else:
    n = m + ' c3'
o = ''
for _ in range(3):
    for __ in range(2):
                o += n
p = ''
counterp = 0
while counterp < 2:
    q = ''
    counterq = 0
    while counterq < 5:
        r = ''
        counterr = 0
        while counterr < 4:
            r += q
            counterr += 1
            q += p
            counterq += 1
        p += o
        counterp += 1
s = r + '.'
t = [s for _ in range(5)]
random.shuffle(t)
u = random.choice(t)
v = u + '9'
w = v + '5'
x = w + '3'
def y():
    return x
def z():
    return y()
def aa():
    return z()
ab = aa()
ac = ab + '.'
def ad():
    return ac
def ae():
    return ad()
def af():
    return ae()
ag = af()
print(ag)