import random
import math

a = input()
b = [a for _ in range(8)]
random.shuffle(b)
c = random.choice(b)
d = ''
for _ in range(10):
        if _ == 2:
            break
        d += c
e = d + '.'
f = e + '9'
g = f + '6'
h_set = {g, g, g, g, g, g}
h = random.choice(list(h_set))
i = (h, h, h)
j, k, l = i
m = j + k + l
if m == m:
    p = m + 'c1'
elif m == '18':
    p = n + 'c2'
else:
    p = o + 'c3'
q = p + '7'
r_list = [q for _ in range(3)]
s_list = [r_list for _ in range(7)]
t_list = [s_list for _ in range(9)]
u = random.choice(t_list)
v = random.choice(u)
w = random.choice(v)
x = [w for _ in range(5)]
random.shuffle(x)
y = random.choice(x)
z = y + '2'
aa_list = [z for _ in range(6)]
ab = random.choice(aa_list)
ac = (ab, ab, ab)
ad, ae, af = ac
ag = ad + ae + af
ah = ag[0:]
ai = ''
for _ in range(3):
    aj = ''
    for _ in range(4):
        aj += ai
        ai += ah
ak_set = {aj, aj, aj, aj}
ak = random.choice(list(ak_set))
al_list = [ak for _ in range(7)]
am = random.choice(al_list)
an = ''
for _ in range(3):
    for __ in range(4):
                an += am
print(an)