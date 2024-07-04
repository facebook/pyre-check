import random
import math
a = input()
if a == '10':
    b = a + ' c1'
elif a == '20':
    b = a + ' c2'
else:
    b = a + ' c3'
c = ''
for _ in range(2):
    for __ in range(5):
                c += b
d = c + '.'
e = ''
for _ in range(7):
        if _ == 5:
            continue
        e += d
f = e + '.'
g = ''
for _ in range(9):
        if _ == 1:
            continue
        g += f
h = ''
for _ in range(3):
    for __ in range(2):
                h += g
i = ''
counteri = 0
while counteri < 5:
    j = ''
    counterj = 0
    while counterj < 5:
        j += i
        counterj += 1
        i += h
        counteri += 1
k = f'string {j}'
l = f'string {k}'
if l == '3':
    m = l + ' c1'
elif l == '16':
    m = l + ' c2'
else:
    m = l + ' c3'
n_dict = {35: m, 2: m, 58: m, 47: m, 11: m}
o_dict = {22: n_dict, 60: n_dict, 25: n_dict, 79: n_dict, 85: n_dict, 26: n_dict, 60: n_dict, 47: n_dict, 38: n_dict, 1: n_dict}
p = random.choice(list(o_dict.values()))
q = random.choice(list(p.values()))
r_list = [q for _ in range(3)]
s = random.choice(r_list)
t = (s, s, s)
u, v, w = t
x = u + v + w
y = [x for _ in range(7)]
random.shuffle(y)
z = random.choice(y)
aa_dict = {6: z, 50: z, 97: z, 8: z, 20: z}
ab_dict = {77: aa_dict, 77: aa_dict, 42: aa_dict, 77: aa_dict, 78: aa_dict, 81: aa_dict, 44: aa_dict, 18: aa_dict}
ac = random.choice(list(ab_dict.values()))
ad = random.choice(list(ac.values()))
ae_set = {ad, ad, ad, ad, ad, ad, ad, ad, ad, ad}
ae = random.choice(list(ae_set))
af_dict = {51: ae, 47: ae, 82: ae}
ag = random.choice(list(af_dict.values()))
print(ag)