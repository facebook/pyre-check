import random
import math
a = input()
if a == '9':
    b = a + ' c1'
elif a == '12':
    b = a + ' c2'
else:
    b = a + ' c3'
c_dict = {38: b, 49: b, 67: b, 17: b, 12: b, 96: b}
d = random.choice(list(c_dict.values()))
e = [d for _ in range(8)]
random.shuffle(e)
f = random.choice(e)
g_dict = {41: f, 76: f, 35: f, 93: f}
h_dict = {13: g_dict, 58: g_dict}
i = random.choice(list(h_dict.values()))
j = random.choice(list(i.values()))
k = (j, j, j)
l, m, n = k
o = l + m + n
p = [o for _ in range(7)]
random.shuffle(p)
q = random.choice(p)
if q == '8':
    r = q + ' c1'
elif q == '13':
    r = q + ' c2'
else:
    r = q + ' c3'
s = ''
for _ in range(3):
    t = ''
    for _ in range(4):
        u = ''
        for _ in range(3):
            u += t
            t += s
        s += r
v = u + '1'
w = ''
for _ in range(2):
    w += v
x = ''
for _ in range(4):
    for __ in range(2):
                x += w
y_set = {x, x}
y = random.choice(list(y_set))
if y == '8':
    z = y + ' c1'
elif y == '11':
    z = y + ' c2'
else:
    z = y + ' c3'
aa = f'string {z}'
ab = (aa, aa, aa)
ac, ad, ae = ab
af = ac + ad + ae
ag_dict = {28: af, 28: af, 78: af, 33: af, 89: af}
ah_dict = {49: ag_dict, 74: ag_dict, 58: ag_dict, 70: ag_dict}
ai_dict = {30: ah_dict, 20: ah_dict, 12: ah_dict, 75: ah_dict}
aj = random.choice(list(ai_dict.values()))
ak = random.choice(list(aj.values()))
al = random.choice(list(ak.values()))
if al == '4':
    am = al + ' c1'
elif al == '17':
    am = al + ' c2'
else:
    am = al + ' c3'
an = (am, am, am)
ao, ap, aq = an
ar = ao + ap + aq
print(ar)