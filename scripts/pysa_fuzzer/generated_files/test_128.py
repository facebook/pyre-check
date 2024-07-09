import random
import math

a = input()
b_set = {a, a, a}
b = random.choice(list(b_set))
c = ''
counterc = 0
while counterc < 4:
    c += b
    counterc += 1
d = c + '7'
e = d + '5'
f = e + '2'
def g():
    return f
h = g()
i = h + '6'
j = i + '4'
k = j + '5'
l_set = {k, k, k, k, k, k, k, k, k, k}
l = random.choice(list(l_set))
m_list = [l for _ in range(6)]
n = random.choice(m_list)
o = ''
for _ in range(3):
    for __ in range(5):
                o += n
p = ''
for _ in range(3):
    for __ in range(2):
                p += o
def q():
    return p
r = q()
s = ''
for _ in range(6):
        if _ == 3:
            continue
        s += r
t_list = [s for _ in range(7)]
u_list = [t_list for _ in range(7)]
v_list = [u_list for _ in range(10)]
w = random.choice(v_list)
x = random.choice(w)
y = random.choice(x)
z = ''
counterz = 0
while counterz < 3:
    aa = ''
    counteraa = 0
    while counteraa < 3:
        ab = ''
        counterab = 0
        while counterab < 2:
            ab += aa
            counterab += 1
            aa += z
            counteraa += 1
        z += y
        counterz += 1
ac = ''
counterac = 0
while counterac < 3:
    ac += ab
    counterac += 1
ad = ''
for _ in range(5):
    ae = ''
    for _ in range(4):
        ae += ad
        ad += ac
af = ''
for _ in range(8):
        if _ == 4:
            break
        af += ae
ag = ''
for _ in range(5):
    for __ in range(4):
                ag += af
ah = ''
for _ in range(3):
    ah += ag
print(ah)