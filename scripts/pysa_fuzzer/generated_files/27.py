import random
import math
a = input()
b_list = [a for _ in range(9)]
c = random.choice(b_list)
d = f'string {c}'
e = d[0:]
f = ''
for _ in range(8):
        if _ == 5:
            break
        f += e
g = [f for _ in range(7)]
random.shuffle(g)
h = random.choice(g)
i = ''
for _ in range(2):
    for __ in range(5):
                i += h
def j():
    return i
k = j()
if k == '6':
    l = k + ' c1'
elif k == '12':
    l = k + ' c2'
else:
    l = k + ' c3'
m_list = [l for _ in range(9)]
n_list = [m_list for _ in range(4)]
o = random.choice(n_list)
p = random.choice(o)
q = ''
counterq = 0
while counterq < 2:
    r = ''
    counterr = 0
    while counterr < 3:
        r += q
        counterr += 1
        q += p
        counterq += 1
s = f'string {r}'
t = s[0:]
u = f'string {t}'
v = u[0:]
w = [v for _ in range(6)]
random.shuffle(w)
x = random.choice(w)
def y():
    return x
def z():
    return y()
def aa():
    return z()
ab = aa()
ac_list = [ab for _ in range(5)]
ad = random.choice(ac_list)
def ae():
    return ad
af = ae()
print(af)