import random
import math
a = input()
b = ''
for _ in range(5):
        if _ == 3:
            break
        b += a
c = ''
counterc = 0
while counterc < 3:
    d = ''
    counterd = 0
    while counterd < 4:
        e = ''
        countere = 0
        while countere < 4:
            e += d
            countere += 1
            d += c
            counterd += 1
        c += b
        counterc += 1
def f():
    return e
def g():
    return f()
def h():
    return g()
i = h()
j_list = [i for _ in range(2)]
k_list = [j_list for _ in range(5)]
l = random.choice(k_list)
m = random.choice(l)
n = m + '4'
o = n + '1'
p = o + '7'
q_set = {p, p, p, p, p, p}
q = random.choice(list(q_set))
r = (q, q, q)
s, t, u = r
v = s + t + u
w = v + '.'
x = [w for _ in range(9)]
random.shuffle(x)
y = random.choice(x)
def z():
    return y
aa = z()
ab = ''
for _ in range(4):
    for __ in range(2):
                ab += aa
ac = ab[0:]
if ac == '2':
    ad = ac + ' c1'
elif ac == '19':
    ad = ac + ' c2'
else:
    ad = ac + ' c3'
ae_set = {ad, ad, ad, ad, ad, ad}
ae = random.choice(list(ae_set))
af = ae + '.'
ag = ''
counterag = 0
while counterag < 5:
    ah = ''
    counterah = 0
    while counterah < 2:
        ah += ag
        counterah += 1
        ag += af
        counterag += 1
ai = ''
for _ in range(2):
    ai += ah
aj = [ai for _ in range(5)]
random.shuffle(aj)
ak = random.choice(aj)
print(ak)