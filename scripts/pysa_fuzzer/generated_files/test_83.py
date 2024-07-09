import random
import math

a = input()
b = ''
for _ in range(3):
    for __ in range(3):
                b += a
c_dict = {38: b, 43: b, 16: b}
d_dict = {61: c_dict, 53: c_dict, 94: c_dict, 85: c_dict, 28: c_dict}
e = random.choice(list(d_dict.values()))
f = random.choice(list(e.values()))
g = f + '1'
h = g + '9'
i = ''
for _ in range(5):
        if _ == 1:
            continue
        i += h
j = ''
for _ in range(4):
    for __ in range(5):
                j += i
k = j + '.'
def l():
    return k
def m():
    return l()
def n():
    return m()
o = n()
def p():
    return o
q = p()
r = ''
counterr = 0
while counterr < 5:
    s = ''
    counters = 0
    while counters < 3:
        s += r
        counters += 1
        r += q
        counterr += 1
t = s[0:]
def u():
    return t
v = u()
w = v + '.'
x_set = {w, w, w, w, w, w}
x = random.choice(list(x_set))
y = x + '.'
z_set = {y, y}
z = random.choice(list(z_set))
aa_list = [z for _ in range(5)]
ab = random.choice(aa_list)
ac_list = [ab for _ in range(8)]
ad_list = [ac_list for _ in range(7)]
ae = random.choice(ad_list)
af = random.choice(ae)
if af == af:
    ai = af + 'c1'
elif af == '14':
    ai = ag + 'c2'
else:
    ai = ah + 'c3'
print(ai)