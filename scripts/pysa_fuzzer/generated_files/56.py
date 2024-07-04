import random
import math
a = input()
b = ''
for _ in range(3):
    for __ in range(3):
                b += a
c = ''
for _ in range(9):
        if _ == 2:
            continue
        c += b
d = c + '2'
e = d + '6'
f_dict = {96: e, 39: e, 24: e, 77: e, 64: e, 50: e, 62: e, 37: e, 66: e}
g_dict = {10: f_dict, 84: f_dict}
h = random.choice(list(g_dict.values()))
i = random.choice(list(h.values()))
if i == '8':
    j = i + ' c1'
elif i == '12':
    j = i + ' c2'
else:
    j = i + ' c3'
k = j + '6'
l = [k for _ in range(8)]
random.shuffle(l)
m = random.choice(l)
def n():
    return m
def o():
    return n()
def p():
    return o()
q = p()
r = ''
counterr = 0
while counterr < 2:
    s = ''
    counters = 0
    while counters < 4:
        s += r
        counters += 1
        r += q
        counterr += 1
t = s + '5'
u = t + '9'
v = u + '7'
w = [v for _ in range(9)]
random.shuffle(w)
x = random.choice(w)
y = x[0:]
z = f'string {y}'
aa = z + '4'
ab = aa + '1'
ac = ''
for _ in range(5):
    for __ in range(2):
                ac += ab
ad = ''
for _ in range(2):
    ad += ac
ae_dict = {38: ad, 27: ad, 19: ad, 53: ad, 55: ad, 93: ad, 84: ad}
af = random.choice(list(ae_dict.values()))
ag = ''
counterag = 0
while counterag < 4:
    ah = ''
    counterah = 0
    while counterah < 2:
        ah += ag
        counterah += 1
        ag += af
        counterag += 1
print(ah)