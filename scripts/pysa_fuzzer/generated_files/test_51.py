import random
import math

a = input()
b_set = {a, a, a}
b = random.choice(list(b_set))
c = b + '7'
d = c + '5'
e = d + '9'
def f():
    return e
def g():
    return f()
h = g()
i_list = [h for _ in range(8)]
j_list = [i_list for _ in range(6)]
k_list = [j_list for _ in range(4)]
l = random.choice(k_list)
m = random.choice(l)
n = random.choice(m)
o = (n, n, n)
p, q, r = o
s = p + q + r
if s == s:
    v = s + 'c1'
elif s == '20':
    v = t + 'c2'
else:
    v = u + 'c3'
w = ''
counterw = 0
while counterw < 3:
    x = ''
    counterx = 0
    while counterx < 3:
        x += w
        counterx += 1
        w += v
        counterw += 1
y = ''
for _ in range(2):
    for __ in range(3):
                y += x
z_list = [y for _ in range(3)]
aa = random.choice(z_list)
ab = aa + '.'
ac = f'string {ab}'
ad = ac[0:]
if ad == ad:
    ag = ad + 'c1'
elif ad == '15':
    ag = ae + 'c2'
else:
    ag = af + 'c3'
ah = ''
for _ in range(4):
    ai = ''
    for _ in range(2):
        aj = ''
        for _ in range(5):
            aj += ai
            ai += ah
        ah += ag
ak = aj[0:]
al = f'string {ak}'
am = (al, al, al)
an, ao, ap = am
aq = an + ao + ap
ar = ''
for _ in range(10):
        if _ == 2:
            break
        ar += aq
print(ar)