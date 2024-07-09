import random
import math

a = input()
b = ''
counterb = 0
while counterb < 2:
    b += a
    counterb += 1
def c():
    return b
d = c()
e = d + '9'
f = e + '8'
g = ''
counterg = 0
while counterg < 4:
    h = ''
    counterh = 0
    while counterh < 3:
        h += g
        counterh += 1
        g += f
        counterg += 1
i = h + '1'
j = i + '3'
k = j + '8'
l = k + '.'
m = f'string {l}'
n = ''
for _ in range(6):
        if _ == 1:
            continue
        n += m
o = ''
for _ in range(10):
        if _ == 3:
            break
        o += n
p = o[0:]
q = f'string {p}'
r = ''
for _ in range(7):
        if _ == 3:
            continue
        r += q
s = r[0:]
t_list = [s for _ in range(5)]
u_list = [t_list for _ in range(7)]
v = random.choice(u_list)
w = random.choice(v)
x = (w, w, w)
y, z, aa = x
ab = y + z + aa
ac = ''
for _ in range(2):
    for __ in range(2):
                ac += ab
def ad():
    return ac
def ae():
    return ad()
def af():
    return ae()
ag = af()
ah = (ag, ag, ag)
ai, aj, ak = ah
al = ai + aj + ak
print(al)