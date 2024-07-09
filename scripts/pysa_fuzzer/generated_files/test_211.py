import random
import math

a = input()
b = ''
counterb = 0
while counterb < 4:
    b += a
    counterb += 1
c = [b for _ in range(6)]
random.shuffle(c)
d = random.choice(c)
e = ''
for _ in range(6):
        if _ == 3:
            continue
        e += d
f_dict = {58: e, 64: e, 88: e, 40: e, 15: e}
g_dict = {96: f_dict, 16: f_dict, 13: f_dict, 57: f_dict, 11: f_dict, 64: f_dict, 70: f_dict}
h_dict = {24: g_dict, 95: g_dict, 53: g_dict, 81: g_dict, 64: g_dict, 65: g_dict, 58: g_dict}
i = random.choice(list(h_dict.values()))
j = random.choice(list(i.values()))
k = random.choice(list(j.values()))
l = k[0:]
m = ''
counterm = 0
while counterm < 2:
    m += l
    counterm += 1
n_list = [m for _ in range(4)]
o = random.choice(n_list)
if o == o:
    r = o + 'c1'
elif o == '19':
    r = p + 'c2'
else:
    r = q + 'c3'
s = f'string {r}'
t = (s, s, s)
u, v, w = t
x = u + v + w
y = x[0:]
z_set = {y, y, y, y, y, y, y, y, y}
z = random.choice(list(z_set))
aa = ''
for _ in range(4):
    ab = ''
    for _ in range(4):
        ac = ''
        for _ in range(2):
            ac += ab
            ab += aa
        aa += z
ad = ''
for _ in range(5):
    for __ in range(3):
                ad += ac
ae = ''
for _ in range(4):
    for __ in range(3):
                ae += ad
af_dict = {13: ae, 60: ae, 14: ae, 56: ae, 85: ae, 34: ae, 82: ae}
ag = random.choice(list(af_dict.values()))
ah = ''
for _ in range(7):
        if _ == 5:
            break
        ah += ag
ai = f'string {ah}'
print(ai)