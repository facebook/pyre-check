import random
import math

a = input()
b = ''
counterb = 0
while counterb < 4:
    b += a
    counterb += 1
c_list = [b for _ in range(5)]
d_list = [c_list for _ in range(2)]
e = random.choice(d_list)
f = random.choice(e)
g = f[0:]
h_set = {g, g, g}
h = random.choice(list(h_set))
i = ''
for _ in range(4):
    for __ in range(5):
                i += h
j_dict = {88: i, 87: i, 48: i, 24: i}
k_dict = {67: j_dict, 51: j_dict, 91: j_dict, 75: j_dict}
l = random.choice(list(k_dict.values()))
m = random.choice(list(l.values()))
def n():
    return m
def o():
    return n()
p = o()
q = p + '8'
r_list = [q for _ in range(3)]
s_list = [r_list for _ in range(8)]
t = random.choice(s_list)
u = random.choice(t)
v = f'string {u}'
w = f'string {v}'
if w == w:
    z = w + 'c1'
elif w == '17':
    z = x + 'c2'
else:
    z = y + 'c3'
aa = z + '.'
ab = aa[0:]
ac = ab[0:]
ad = ''
for _ in range(4):
    ae = ''
    for _ in range(2):
        ae += ad
        ad += ac
af = ''
counteraf = 0
while counteraf < 5:
    ag = ''
    counterag = 0
    while counterag < 4:
        ag += af
        counterag += 1
        af += ae
        counteraf += 1
ah = ''
for _ in range(8):
        if _ == 4:
            continue
        ah += ag
print(ah)