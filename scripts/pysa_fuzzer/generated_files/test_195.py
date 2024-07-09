import random
import math

a = input()
b = a + '2'
c = b + '8'
d = c + '9'
if d == d:
    g = d + 'c1'
elif d == '13':
    g = e + 'c2'
else:
    g = f + 'c3'
h = [g for _ in range(10)]
random.shuffle(h)
i = random.choice(h)
j_dict = {55: i, 16: i}
k_dict = {81: j_dict, 82: j_dict, 31: j_dict, 45: j_dict, 50: j_dict, 23: j_dict, 100: j_dict, 25: j_dict, 12: j_dict}
l = random.choice(list(k_dict.values()))
m = random.choice(list(l.values()))
n = m + '3'
o = n + '4'
p = o + '.'
q_set = {p, p, p, p}
q = random.choice(list(q_set))
r = f'string {q}'
s_list = [r for _ in range(3)]
t_list = [s_list for _ in range(9)]
u = random.choice(t_list)
v = random.choice(u)
w = ''
for _ in range(8):
        if _ == 5:
            continue
        w += v
x = ''
for _ in range(4):
    y = ''
    for _ in range(3):
        z = ''
        for _ in range(3):
            z += y
            y += x
        x += w
aa = ''
counteraa = 0
while counteraa < 2:
    aa += z
    counteraa += 1
ab = ''
for _ in range(2):
    for __ in range(5):
                ab += aa
ac = ''
for _ in range(2):
    ad = ''
    for _ in range(2):
        ad += ac
        ac += ab
ae = ''
counterae = 0
while counterae < 4:
    ae += ad
    counterae += 1
af_set = {ae, ae}
af = random.choice(list(af_set))
ag_dict = {12: af, 65: af, 15: af}
ah = random.choice(list(ag_dict.values()))
print(ah)