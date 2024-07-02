import random
import math
a = input()
b_set = {a, a, a, a, a}
b = random.choice(list(b_set))
c = f'string {b}'
d = c + '.'
e = ''
for _ in range(2):
    for __ in range(3):
                e += d
f = e[0:]
g = (f, f, f)
h, i, j = g
k = h + i + j
l = k[0:]
m = l + '3'
n = m + '9'
o = ''
for _ in range(3):
    for __ in range(4):
                o += n
p_list = [o for _ in range(6)]
q_list = [p_list for _ in range(3)]
r_list = [q_list for _ in range(3)]
s = random.choice(r_list)
t = random.choice(s)
u = random.choice(t)
v = ''
for _ in range(5):
        if _ == 1:
            break
        v += u
w_list = [v for _ in range(2)]
x_list = [w_list for _ in range(4)]
y = random.choice(x_list)
z = random.choice(y)
aa = ''
for _ in range(4):
    aa += z
def ab():
    return aa
def ac():
    return ab()
def ad():
    return ac()
ae = ad()
af = ''
for _ in range(4):
    ag = ''
    for _ in range(2):
        ag += af
        af += ae
ah = ''
for _ in range(3):
    ai = ''
    for _ in range(2):
        aj = ''
        for _ in range(4):
            aj += ai
            ai += ah
        ah += ag
ak = ''
counterak = 0
while counterak < 4:
    al = ''
    counteral = 0
    while counteral < 5:
        al += ak
        counteral += 1
        ak += aj
        counterak += 1
am = ''
for _ in range(2):
    an = ''
    for _ in range(4):
        an += am
        am += al
print(an)