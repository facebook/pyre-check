import random
import math
a = input()
b = ''
counterb = 0
while counterb < 4:
    c = ''
    counterc = 0
    while counterc < 4:
        c += b
        counterc += 1
        b += a
        counterb += 1
d = ''
counterd = 0
while counterd < 4:
    d += c
    counterd += 1
e = d + '5'
f = e + '4'
g_dict = {45: f, 13: f}
h = random.choice(list(g_dict.values()))
i_set = {h, h, h, h, h, h, h, h, h, h}
i = random.choice(list(i_set))
j_set = {i, i, i, i, i, i, i}
j = random.choice(list(j_set))
k = ''
for _ in range(3):
    for __ in range(5):
                k += j
l = ''
for _ in range(7):
        if _ == 1:
            continue
        l += k
m_list = [l for _ in range(7)]
n_list = [m_list for _ in range(7)]
o = random.choice(n_list)
p = random.choice(o)
q = f'string {p}'
r = [q for _ in range(10)]
random.shuffle(r)
s = random.choice(r)
t = s + '5'
u = t + '6'
v = u + '5'
def w():
    return v
def x():
    return w()
def y():
    return x()
z = y()
aa_set = {z, z, z, z}
aa = random.choice(list(aa_set))
ab = ''
for _ in range(2):
    ab += aa
ac = ''
for _ in range(2):
    ad = ''
    for _ in range(2):
        ae = ''
        for _ in range(2):
            ae += ad
            ad += ac
        ac += ab
af = ''
for _ in range(7):
        if _ == 2:
            continue
        af += ae
ag = af + '2'
print(ag)