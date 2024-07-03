import random
import math
a = input()
b_set = {a, a}
b = random.choice(list(b_set))
def c():
    return b
def d():
    return c()
e = d()
f = e[0:]
g = ''
for _ in range(6):
        if _ == 5:
            break
        g += f
h = ''
for _ in range(8):
        if _ == 4:
            continue
        h += g
i_list = [h for _ in range(5)]
j = random.choice(i_list)
k = ''
for _ in range(5):
    k += j
l = (k, k, k)
m, n, o = l
p = m + n + o
q = ''
for _ in range(9):
        if _ == 4:
            continue
        q += p
r_list = [q for _ in range(2)]
s_list = [r_list for _ in range(5)]
t = random.choice(s_list)
u = random.choice(t)
def v():
    return u
def w():
    return v()
def x():
    return w()
y = x()
z_set = {y, y, y, y, y, y, y, y, y, y}
z = random.choice(list(z_set))
aa = ''
for _ in range(2):
    ab = ''
    for _ in range(4):
        ab += aa
        aa += z
ac = ''
for _ in range(3):
    ad = ''
    for _ in range(5):
        ae = ''
        for _ in range(4):
            ae += ad
            ad += ac
        ac += ab
af = ''
for _ in range(5):
    ag = ''
    for _ in range(3):
        ah = ''
        for _ in range(3):
            ah += ag
            ag += af
        af += ae
ai = ''
for _ in range(3):
    aj = ''
    for _ in range(2):
        aj += ai
        ai += ah
ak_set = {aj, aj, aj, aj, aj, aj, aj, aj, aj}
ak = random.choice(list(ak_set))
al = [ak for _ in range(6)]
random.shuffle(al)
am = random.choice(al)
print(am)