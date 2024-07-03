import random
import math
a = input()
b = [a for _ in range(10)]
random.shuffle(b)
c = random.choice(b)
d = (c, c, c)
e, f, g = d
h = e + f + g
i = ''
for _ in range(5):
    for __ in range(3):
                i += h
j = ''
for _ in range(2):
    j += i
k = j + '.'
l = k + '.'
m = f'string {l}'
n = m[0:]
o = [n for _ in range(9)]
random.shuffle(o)
p = random.choice(o)
q_dict = {6: p, 37: p}
r = random.choice(list(q_dict.values()))
if r == '5':
    s = r + ' c1'
elif r == '20':
    s = r + ' c2'
else:
    s = r + ' c3'
t = (s, s, s)
u, v, w = t
x = u + v + w
y_dict = {70: x, 28: x, 87: x, 55: x, 16: x}
z_dict = {52: y_dict, 61: y_dict, 84: y_dict, 57: y_dict, 87: y_dict, 74: y_dict, 90: y_dict, 38: y_dict, 18: y_dict, 85: y_dict}
aa_dict = {47: z_dict, 71: z_dict, 8: z_dict, 23: z_dict, 11: z_dict}
ab = random.choice(list(aa_dict.values()))
ac = random.choice(list(ab.values()))
ad = random.choice(list(ac.values()))
ae = ad + '6'
af = ae + '1'
ag_list = [af for _ in range(6)]
ah_list = [ag_list for _ in range(5)]
ai_list = [ah_list for _ in range(9)]
aj = random.choice(ai_list)
ak = random.choice(aj)
al = random.choice(ak)
if al == '8':
    am = al + ' c1'
elif al == '20':
    am = al + ' c2'
else:
    am = al + ' c3'
an = (am, am, am)
ao, ap, aq = an
ar = ao + ap + aq
at = ''
for _ in range(3):
    for __ in range(4):
                at += ar
print(at)