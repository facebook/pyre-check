import random
import math

a = input()
b_dict = {58: a, 79: a, 8: a, 79: a, 19: a, 39: a}
c_dict = {13: b_dict, 46: b_dict, 100: b_dict, 41: b_dict, 22: b_dict}
d_dict = {2: c_dict, 94: c_dict, 31: c_dict, 4: c_dict, 42: c_dict, 93: c_dict, 49: c_dict, 85: c_dict}
e = random.choice(list(d_dict.values()))
f = random.choice(list(e.values()))
g = random.choice(list(f.values()))
h = f'string {g}'
i = [h for _ in range(8)]
random.shuffle(i)
j = random.choice(i)
k_set = {j, j, j}
k = random.choice(list(k_set))
l = k[0:]
m = ''
for _ in range(2):
    m += l
n_list = [m for _ in range(4)]
o_list = [n_list for _ in range(5)]
p = random.choice(o_list)
q = random.choice(p)
r = [q for _ in range(10)]
random.shuffle(r)
s = random.choice(r)
t = s[0:]
if t == t:
    w = t + 'c1'
elif t == '12':
    w = u + 'c2'
else:
    w = v + 'c3'
x_set = {w, w, w, w, w, w, w, w}
x = random.choice(list(x_set))
y = x[0:]
z = ''
for _ in range(9):
        if _ == 3:
            continue
        z += y
aa = ''
for _ in range(5):
        if _ == 5:
            continue
        aa += z
ab = f'string {aa}'
ac = ''
counterac = 0
while counterac < 4:
    ad = ''
    counterad = 0
    while counterad < 5:
        ae = ''
        counterae = 0
        while counterae < 4:
            ae += ad
            counterae += 1
            ad += ac
            counterad += 1
        ac += ab
        counterac += 1
af_set = {ae, ae, ae, ae, ae}
af = random.choice(list(af_set))
ag = ''
for _ in range(2):
    for __ in range(2):
                ag += af
print(ag)