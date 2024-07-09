import random
import math

a = input()
if a == a:
    d = a + 'c1'
elif a == '14':
    d = b + 'c2'
else:
    d = c + 'c3'
e = ''
countere = 0
while countere < 3:
    f = ''
    counterf = 0
    while counterf < 4:
        g = ''
        counterg = 0
        while counterg < 4:
            g += f
            counterg += 1
            f += e
            counterf += 1
        e += d
        countere += 1
h_list = [g for _ in range(6)]
i_list = [h_list for _ in range(3)]
j = random.choice(i_list)
k = random.choice(j)
l = ''
for _ in range(3):
    for __ in range(2):
                l += k
m = ''
for _ in range(7):
        if _ == 5:
            break
        m += l
n = ''
for _ in range(2):
    for __ in range(5):
                n += m
o = (n, n, n)
p, q, r = o
s = p + q + r
def t():
    return s
def u():
    return t()
def v():
    return u()
w = v()
x_dict = {56: w, 66: w, 51: w, 88: w}
y_dict = {57: x_dict, 12: x_dict, 12: x_dict}
z_dict = {88: y_dict, 58: y_dict, 64: y_dict}
aa = random.choice(list(z_dict.values()))
ab = random.choice(list(aa.values()))
ac = random.choice(list(ab.values()))
def ad():
    return ac
ae = ad()
af = ''
counteraf = 0
while counteraf < 3:
    af += ae
    counteraf += 1
ag = ''
for _ in range(4):
    for __ in range(4):
                ag += af
ah = [ag for _ in range(6)]
random.shuffle(ah)
ai = random.choice(ah)
aj = ai[0:]
ak = f'string {aj}'
al = ak + '6'
am = al + '3'
an = am + '5'
ao_dict = {34: an, 12: an, 67: an, 13: an}
ap = random.choice(list(ao_dict.values()))
aq = ''
for _ in range(3):
    for __ in range(5):
                aq += ap
print(aq)