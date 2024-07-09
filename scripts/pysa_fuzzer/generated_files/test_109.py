import random
import math

a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g_set = {f, f}
g = random.choice(list(g_set))
h = g + '2'
i = h + '4'
j = i + '6'
k = j + '5'
l = k + '1'
m_list = [l for _ in range(3)]
n_list = [m_list for _ in range(9)]
o_list = [n_list for _ in range(8)]
p = random.choice(o_list)
q = random.choice(p)
r = random.choice(q)
s = [r for _ in range(5)]
random.shuffle(s)
t = random.choice(s)
u = ''
for _ in range(3):
    for __ in range(3):
                u += t
v = u + '.'
w = v + '.'
x = ''
for _ in range(9):
        if _ == 2:
            break
        x += w
y = x + '6'
z = ''
for _ in range(9):
        if _ == 1:
            continue
        z += y
aa = ''
for _ in range(2):
    for __ in range(3):
                aa += z
if aa == aa:
    ad = aa + 'c1'
elif aa == '12':
    ad = ab + 'c2'
else:
    ad = ac + 'c3'
ae_set = {ad, ad, ad, ad, ad, ad, ad, ad}
ae = random.choice(list(ae_set))
if ae == ae:
    ah = ae + 'c1'
elif ae == '11':
    ah = af + 'c2'
else:
    ah = ag + 'c3'
ai = ''
for _ in range(2):
    aj = ''
    for _ in range(4):
        ak = ''
        for _ in range(5):
            ak += aj
            aj += ai
        ai += ah
al_set = {ak, ak, ak, ak, ak, ak, ak}
al = random.choice(list(al_set))
print(al)