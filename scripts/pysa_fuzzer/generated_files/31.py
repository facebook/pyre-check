import random
import math
a = input()
if a == '5':
    b = a + ' c1'
elif a == '16':
    b = a + ' c2'
else:
    b = a + ' c3'
c = f'string {b}'
d = [c for _ in range(5)]
random.shuffle(d)
e = random.choice(d)
f_dict = {77: e, 40: e}
g_dict = {74: f_dict, 78: f_dict, 15: f_dict, 79: f_dict, 37: f_dict, 93: f_dict, 65: f_dict, 16: f_dict}
h = random.choice(list(g_dict.values()))
i = random.choice(list(h.values()))
j = (i, i, i)
k, l, m = j
n = k + l + m
o = (n, n, n)
p, q, r = o
s = p + q + r
def t():
    return s
def u():
    return t()
v = u()
w = ''
counterw = 0
while counterw < 2:
    x = ''
    counterx = 0
    while counterx < 2:
        y = ''
        countery = 0
        while countery < 5:
            y += x
            countery += 1
            x += w
            counterx += 1
        w += v
        counterw += 1
z = ''
for _ in range(8):
        if _ == 5:
            continue
        z += y
aa = ''
for _ in range(10):
        if _ == 3:
            break
        aa += z
ab = ''
for _ in range(5):
    ab += aa
ac_set = {ab, ab}
ac = random.choice(list(ac_set))
ad = ''
for _ in range(2):
    for __ in range(5):
                ad += ac
ae = ad[0:]
af_set = {ae, ae, ae, ae, ae, ae, ae}
af = random.choice(list(af_set))
ag = af[0:]
ah = ''
for _ in range(4):
    for __ in range(5):
                ah += ag
ai = ah[0:]
print(ai)