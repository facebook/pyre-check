import random
import math
a = input()
b = a + '8'
c = (b, b, b)
d, e, f = c
g = d + e + f
h = g + '.'
i_dict = {75: h, 36: h, 65: h, 80: h, 42: h, 8: h, 73: h, 89: h, 22: h}
j_dict = {85: i_dict, 44: i_dict, 83: i_dict, 38: i_dict, 67: i_dict, 26: i_dict}
k = random.choice(list(j_dict.values()))
l = random.choice(list(k.values()))
m = f'string {l}'
n_dict = {22: m, 38: m, 62: m, 30: m, 79: m, 94: m, 47: m, 14: m}
o_dict = {54: n_dict, 7: n_dict, 94: n_dict, 6: n_dict, 96: n_dict, 85: n_dict, 11: n_dict, 1: n_dict, 47: n_dict, 18: n_dict}
p = random.choice(list(o_dict.values()))
q = random.choice(list(p.values()))
r_set = {q, q, q, q, q}
r = random.choice(list(r_set))
s = r[0:]
t = s + '.'
if t == '8':
    u = t + ' c1'
elif t == '20':
    u = t + ' c2'
else:
    u = t + ' c3'
v = ''
for _ in range(5):
        if _ == 4:
            continue
        v += u
if v == '4':
    w = v + ' c1'
elif v == '20':
    w = v + ' c2'
else:
    w = v + ' c3'
x = [w for _ in range(5)]
random.shuffle(x)
y = random.choice(x)
z = [y for _ in range(7)]
random.shuffle(z)
aa = random.choice(z)
ab = ''
for _ in range(2):
    ab += aa
ac = f'string {ab}'
ad = ac + '6'
ae = ad + '5'
af = ae + '2'
ag = [af for _ in range(6)]
random.shuffle(ag)
ah = random.choice(ag)
print(ah)