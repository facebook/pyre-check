import random
import math
a = input()
b_set = {a, a, a, a, a, a, a, a}
b = random.choice(list(b_set))
c_set = {b, b, b, b}
c = random.choice(list(c_set))
d = c + '.'
e_set = {d, d, d, d, d}
e = random.choice(list(e_set))
f = e + '.'
g = ''
for _ in range(4):
    for __ in range(4):
                g += f
h = g + '.'
if h == '8':
    i = h + ' c1'
elif h == '14':
    i = h + ' c2'
else:
    i = h + ' c3'
if i == '7':
    j = i + ' c1'
elif i == '20':
    j = i + ' c2'
else:
    j = i + ' c3'
k = [j for _ in range(6)]
random.shuffle(k)
l = random.choice(k)
m = ''
for _ in range(2):
    for __ in range(2):
                m += l
n = ''
for _ in range(5):
    for __ in range(3):
                n += m
o = [n for _ in range(7)]
random.shuffle(o)
p = random.choice(o)
q = p[0:]
r = q + '6'
s = ''
for _ in range(4):
    s += r
t = ''
counter = 0
while counter < 5:
    t += s
    counter += 1
u = ''
counter = 0
while counter < 4:
    u += t
    counter += 1
v = (u, u, u)
w, x, y = v
z = w + x + y
aa = f'string {z}'
if aa == '5':
    ab = aa + ' c1'
elif aa == '20':
    ab = aa + ' c2'
else:
    ab = aa + ' c3'
if ab == '10':
    ac = ab + ' c1'
elif ab == '11':
    ac = ab + ' c2'
else:
    ac = ab + ' c3'
ad = ''
for _ in range(5):
    ad += ac
ae = f'string {ad}'
af = ''
for _ in range(5):
    for __ in range(3):
                af += ae
ag_dict = {62: af, 28: af, 36: af}
ag = random.choice(list(ag_dict.values()))
ah_dict = {91: ag, 9: ag, 68: ag}
ah = random.choice(list(ah_dict.values()))
ai_list = [ah for _ in range(3)]
ai = random.choice(ai_list)
aj = (ai, ai, ai)
ak, al, am = aj
an = ak + al + am
ao_set = {an, an, an}
ao = random.choice(list(ao_set))
ap_list = [ao for _ in range(8)]
ap = random.choice(ap_list)
if ap == '6':
    aq = ap + ' c1'
elif ap == '15':
    aq = ap + ' c2'
else:
    aq = ap + ' c3'
if aq == '6':
    ar = aq + ' c1'
elif aq == '16':
    ar = aq + ' c2'
else:
    ar = aq + ' c3'
print(ar)