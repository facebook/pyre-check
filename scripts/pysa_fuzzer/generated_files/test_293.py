import random
import math

a = input()
b = a + '5'
c = b + '2'
d = c + '4'
e_list = [d for _ in range(5)]
f_list = [e_list for _ in range(6)]
g_list = [f_list for _ in range(5)]
h = random.choice(g_list)
i = random.choice(h)
j = random.choice(i)
k = j[0:]
l = ''
for _ in range(5):
        if _ == 3:
            break
        l += k
m = f'string {l}'
n_set = {m, m, m, m, m, m, m, m, m, m}
n = random.choice(list(n_set))
o = ''
for _ in range(10):
        if _ == 5:
            break
        o += n
p = ''
for _ in range(5):
    p += o
q_set = {p, p, p, p, p, p}
q = random.choice(list(q_set))
r = ''
for _ in range(6):
        if _ == 4:
            break
        r += q
s = ''
for _ in range(8):
        if _ == 4:
            continue
        s += r
def t():
    return s
def u():
    return t()
def v():
    return u()
w = v()
x_list = [w for _ in range(5)]
y = random.choice(x_list)
z = ''
counterz = 0
while counterz < 2:
    aa = ''
    counteraa = 0
    while counteraa < 3:
        ab = ''
        counterab = 0
        while counterab < 4:
            ab += aa
            counterab += 1
            aa += z
            counteraa += 1
        z += y
        counterz += 1
ac_list = [ab for _ in range(6)]
ad_list = [ac_list for _ in range(7)]
ae = random.choice(ad_list)
af = random.choice(ae)
ag = ''
for _ in range(5):
    for __ in range(4):
                ag += af
ah = ag[0:]
ai = f'string {ah}'
print(ai)