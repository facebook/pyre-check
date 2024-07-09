import random
import math

a = input()
b = ''
for _ in range(5):
        if _ == 4:
            break
        b += a
def c():
    return b
d = c()
e = d + '6'
f = e + '8'
g = f + '3'
h = g[0:]
i = ''
for _ in range(7):
        if _ == 3:
            break
        i += h
j = (i, i, i)
k, l, m = j
n = k + l + m
o = ''
for _ in range(6):
        if _ == 4:
            continue
        o += n
p = o + '.'
q = ''
for _ in range(4):
    for __ in range(2):
                q += p
r = (q, q, q)
s, t, u = r
v = s + t + u
w = f'string {v}'
x = f'string {w}'
y_list = [x for _ in range(8)]
z_list = [y_list for _ in range(3)]
aa = random.choice(z_list)
ab = random.choice(aa)
ac = ''
counterac = 0
while counterac < 2:
    ad = ''
    counterad = 0
    while counterad < 3:
        ad += ac
        counterad += 1
        ac += ab
        counterac += 1
ae = f'string {ad}'
af = (ae, ae, ae)
ag, ah, ai = af
aj = ag + ah + ai
ak = f'string {aj}'
def al():
    return ak
def am():
    return al()
an = am()
print(an)