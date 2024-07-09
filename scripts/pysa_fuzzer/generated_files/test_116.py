import random
import math

a = input()
b = ''
for _ in range(5):
    c = ''
    for _ in range(4):
        d = ''
        for _ in range(5):
            d += c
            c += b
        b += a
e_dict = {2: d, 35: d}
f_dict = {44: e_dict, 60: e_dict, 5: e_dict, 82: e_dict, 19: e_dict, 76: e_dict, 74: e_dict, 23: e_dict}
g = random.choice(list(f_dict.values()))
h = random.choice(list(g.values()))
i = f'string {h}'
j = i + '.'
if j == j:
    m = j + 'c1'
elif j == '14':
    m = k + 'c2'
else:
    m = l + 'c3'
n_list = [m for _ in range(8)]
o_list = [n_list for _ in range(4)]
p = random.choice(o_list)
q = random.choice(p)
r_list = [q for _ in range(4)]
s = random.choice(r_list)
t = (s, s, s)
u, v, w = t
x = u + v + w
y = x + '4'
z = y + '3'
aa = ''
for _ in range(2):
    aa += z
ab = aa + '.'
ac_set = {ab, ab, ab, ab, ab, ab, ab, ab, ab}
ac = random.choice(list(ac_set))
ad = ac + '.'
ae = ad + '.'
af = ae + '3'
ag = af + '8'
ah = ''
for _ in range(5):
    for __ in range(3):
                ah += ag
ai = (ah, ah, ah)
aj, ak, al = ai
am = aj + ak + al
an = ''
for _ in range(6):
        if _ == 4:
            break
        an += am
print(an)