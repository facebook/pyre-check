import random
import math
a = input()
b = ''
for _ in range(7):
        if _ == 3:
            break
        b += a
c = ''
for _ in range(5):
    for __ in range(3):
                c += b
if c == '8':
    d = c + ' c1'
elif c == '15':
    d = c + ' c2'
else:
    d = c + ' c3'
e = ''
countere = 0
while countere < 2:
    e += d
    countere += 1
def f():
    return e
def g():
    return f()
def h():
    return g()
i = h()
j_list = [i for _ in range(10)]
k = random.choice(j_list)
l = k + '2'
m = l[0:]
n_set = {m, m, m, m, m}
n = random.choice(list(n_set))
o = ''
for _ in range(4):
    p = ''
    for _ in range(5):
        q = ''
        for _ in range(5):
            q += p
            p += o
        o += n
def r():
    return q
def s():
    return r()
t = s()
u = ''
counteru = 0
while counteru < 5:
    v = ''
    counterv = 0
    while counterv < 5:
        v += u
        counterv += 1
        u += t
        counteru += 1
w = f'string {v}'
x = w + '4'
y = x + '1'
z = y + '3'
if z == '2':
    aa = z + ' c1'
elif z == '15':
    aa = z + ' c2'
else:
    aa = z + ' c3'
ab = ''
for _ in range(2):
    ab += aa
ac_dict = {50: ab, 17: ab, 91: ab, 9: ab, 1: ab, 76: ab, 35: ab, 95: ab, 64: ab}
ad_dict = {18: ac_dict, 13: ac_dict, 80: ac_dict, 1: ac_dict, 78: ac_dict, 65: ac_dict}
ae_dict = {97: ad_dict, 23: ad_dict, 6: ad_dict, 28: ad_dict}
af = random.choice(list(ae_dict.values()))
ag = random.choice(list(af.values()))
ah = random.choice(list(ag.values()))
ai = [ah for _ in range(10)]
random.shuffle(ai)
aj = random.choice(ai)
print(aj)