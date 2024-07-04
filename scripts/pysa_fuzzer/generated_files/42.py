import random
import math
a = input()
b = ''
for _ in range(2):
    for __ in range(3):
                b += a
c = f'string {b}'
d = ''
for _ in range(4):
    for __ in range(2):
                d += c
e_dict = {21: d, 94: d}
f_dict = {17: e_dict, 9: e_dict, 37: e_dict, 63: e_dict, 94: e_dict}
g_dict = {39: f_dict, 7: f_dict, 77: f_dict}
h = random.choice(list(g_dict.values()))
i = random.choice(list(h.values()))
j = random.choice(list(i.values()))
k = ''
for _ in range(3):
    for __ in range(4):
                k += j
l = [k for _ in range(6)]
random.shuffle(l)
m = random.choice(l)
n = m + '9'
o = n + '2'
p = o + '8'
q = p + '.'
r = q + '.'
s = r[0:]
t = ''
for _ in range(7):
        if _ == 2:
            break
        t += s
u = t + '8'
v = u + '3'
w_set = {v, v, v, v, v, v, v, v, v, v}
w = random.choice(list(w_set))
def x():
    return w
def y():
    return x()
def z():
    return y()
aa = z()
if aa == '10':
    ab = aa + ' c1'
elif aa == '12':
    ab = aa + ' c2'
else:
    ab = aa + ' c3'
if ab == '8':
    ac = ab + ' c1'
elif ab == '17':
    ac = ab + ' c2'
else:
    ac = ab + ' c3'
ad_set = {ac, ac, ac, ac, ac}
ad = random.choice(list(ad_set))
ae = ''
counterae = 0
while counterae < 5:
    ae += ad
    counterae += 1
print(ae)