import random
import math
a = input()
b = [a for _ in range(9)]
random.shuffle(b)
c = random.choice(b)
d = c + '6'
e = d + '9'
f = e + '6'
g = f'string {f}'
h = ''
for _ in range(7):
        if _ == 4:
            continue
        h += g
i_list = [h for _ in range(9)]
j = random.choice(i_list)
k_set = {j, j, j, j}
k = random.choice(list(k_set))
l = ''
for _ in range(4):
    m = ''
    for _ in range(3):
        n = ''
        for _ in range(3):
            n += m
            m += l
        l += k
def o():
    return n
def p():
    return o()
q = p()
r_set = {q, q, q, q, q, q, q, q, q, q}
r = random.choice(list(r_set))
s = r[0:]
t_set = {s, s, s, s, s, s, s}
t = random.choice(list(t_set))
u = ''
for _ in range(7):
        if _ == 4:
            break
        u += t
v = (u, u, u)
w, x, y = v
z = w + x + y
aa = f'string {z}'
ab = ''
for _ in range(10):
        if _ == 4:
            break
        ab += aa
ac_list = [ab for _ in range(5)]
ad_list = [ac_list for _ in range(3)]
ae_list = [ad_list for _ in range(2)]
af = random.choice(ae_list)
ag = random.choice(af)
ah = random.choice(ag)
def ai():
    return ah
def aj():
    return ai()
ak = aj()
al = ''
for _ in range(5):
    al += ak
print(al)