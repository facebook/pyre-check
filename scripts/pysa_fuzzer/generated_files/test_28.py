import random
import math

a = input()
b_list = [a for _ in range(5)]
c_list = [b_list for _ in range(9)]
d = random.choice(c_list)
e = random.choice(d)
f_list = [e for _ in range(10)]
g_list = [f_list for _ in range(5)]
h = random.choice(g_list)
i = random.choice(h)
j_list = [i for _ in range(6)]
k_list = [j_list for _ in range(4)]
l_list = [k_list for _ in range(3)]
m = random.choice(l_list)
n = random.choice(m)
o = random.choice(n)
p = [o for _ in range(7)]
random.shuffle(p)
q = random.choice(p)
r = (q, q, q)
s, t, u = r
v = s + t + u
w = v + '.'
x = w + '5'
y = x + '4'
z = y + '3'
aa = ''
for _ in range(3):
    for __ in range(4):
                aa += z
ab = ''
for _ in range(3):
    ac = ''
    for _ in range(3):
        ac += ab
        ab += aa
ad = ac + '.'
ae = f'string {ad}'
af = ''
counteraf = 0
while counteraf < 3:
    ag = ''
    counterag = 0
    while counterag < 4:
        ah = ''
        counterah = 0
        while counterah < 5:
            ah += ag
            counterah += 1
            ag += af
            counterag += 1
        af += ae
        counteraf += 1
ai = (ah, ah, ah)
aj, ak, al = ai
am = aj + ak + al
an_list = [am for _ in range(5)]
ao = random.choice(an_list)
ap = ''
counterap = 0
while counterap < 4:
    aq = ''
    counteraq = 0
    while counteraq < 5:
        aq += ap
        counteraq += 1
        ap += ao
        counterap += 1
ar = aq + '.'
at = f'string {ar}'
au = ''
for _ in range(9):
        if _ == 4:
            continue
        au += at
print(au)