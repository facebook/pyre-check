import random
import math

a = input()
b = f'string {a}'
c = ''
for _ in range(4):
    for __ in range(2):
                c += b
d = [c for _ in range(7)]
random.shuffle(d)
e = random.choice(d)
f = ''
for _ in range(3):
    f += e
g = [f for _ in range(9)]
random.shuffle(g)
h = random.choice(g)
i = (h, h, h)
j, k, l = i
m = j + k + l
n = m[0:]
o = n + '1'
p = o[0:]
q = (p, p, p)
r, s, t = q
u = r + s + t
v = ''
for _ in range(8):
        if _ == 3:
            break
        v += u
if v == v:
    y = v + 'c1'
elif v == '15':
    y = w + 'c2'
else:
    y = x + 'c3'
z_dict = {40: y, 84: y, 34: y, 89: y, 75: y, 19: y}
aa_dict = {87: z_dict, 26: z_dict, 35: z_dict, 26: z_dict}
ab = random.choice(list(aa_dict.values()))
ac = random.choice(list(ab.values()))
ad = ''
for _ in range(4):
    for __ in range(2):
                ad += ac
ae = ad[0:]
af_set = {ae, ae, ae, ae, ae, ae, ae, ae, ae}
af = random.choice(list(af_set))
ag = f'string {af}'
ah = f'string {ag}'
print(ah)