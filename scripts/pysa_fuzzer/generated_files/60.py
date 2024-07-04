import random
import math
a = input()
b = ''
for _ in range(8):
        if _ == 4:
            continue
        b += a
c_set = {b, b, b, b, b, b, b, b}
c = random.choice(list(c_set))
d = ''
for _ in range(4):
    d += c
e = ''
countere = 0
while countere < 4:
    e += d
    countere += 1
if e == '3':
    f = e + ' c1'
elif e == '12':
    f = e + ' c2'
else:
    f = e + ' c3'
g = f'string {f}'
h = (g, g, g)
i, j, k = h
l = i + j + k
def m():
    return l
def n():
    return m()
def o():
    return n()
p = o()
q_dict = {85: p, 33: p, 19: p, 7: p, 75: p, 88: p, 49: p, 74: p, 77: p}
r = random.choice(list(q_dict.values()))
def s():
    return r
def t():
    return s()
u = t()
v_set = {u, u}
v = random.choice(list(v_set))
w = f'string {v}'
x = w + '2'
y = x + '6'
z = (y, y, y)
aa, ab, ac = z
ad = aa + ab + ac
def ae():
    return ad
def af():
    return ae()
def ag():
    return af()
ah = ag()
ai = ''
for _ in range(2):
    aj = ''
    for _ in range(3):
        ak = ''
        for _ in range(3):
            ak += aj
            aj += ai
        ai += ah
al = f'string {ak}'
am = (al, al, al)
an, ao, ap = am
aq = an + ao + ap
print(aq)