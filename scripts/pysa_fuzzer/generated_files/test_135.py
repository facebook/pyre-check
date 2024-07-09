import random
import math

a = input()
b = ''
counterb = 0
while counterb < 5:
    c = ''
    counterc = 0
    while counterc < 5:
        d = ''
        counterd = 0
        while counterd < 4:
            d += c
            counterd += 1
            c += b
            counterc += 1
        b += a
        counterb += 1
e = d + '4'
f = e + '5'
g = f + '4'
h_list = [g for _ in range(10)]
i = random.choice(h_list)
j_set = {i, i, i}
j = random.choice(list(j_set))
k = ''
for _ in range(4):
    k += j
l = k[0:]
m_list = [l for _ in range(5)]
n_list = [m_list for _ in range(8)]
o = random.choice(n_list)
p = random.choice(o)
if p == p:
    s = p + 'c1'
elif p == '19':
    s = q + 'c2'
else:
    s = r + 'c3'
t = f'string {s}'
u_list = [t for _ in range(2)]
v_list = [u_list for _ in range(8)]
w = random.choice(v_list)
x = random.choice(w)
y = ''
for _ in range(3):
    z = ''
    for _ in range(4):
        aa = ''
        for _ in range(3):
            aa += z
            z += y
        y += x
def ab():
    return aa
def ac():
    return ab()
def ad():
    return ac()
ae = ad()
af = ''
for _ in range(3):
    af += ae
ag = ''
for _ in range(3):
    for __ in range(2):
                ag += af
ah = ag + '.'
ai = ''
for _ in range(2):
    for __ in range(3):
                ai += ah
aj = ''
counteraj = 0
while counteraj < 2:
    aj += ai
    counteraj += 1
ak = ''
for _ in range(2):
    for __ in range(2):
                ak += aj
print(ak)