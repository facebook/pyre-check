import random
import math

a = input()
b_set = {a, a, a, a}
b = random.choice(list(b_set))
c = b + '6'
d = c + '1'
e_dict = {34: d, 27: d, 83: d, 95: d, 40: d}
f_dict = {42: e_dict, 64: e_dict}
g_dict = {54: f_dict, 97: f_dict, 79: f_dict, 25: f_dict, 80: f_dict, 51: f_dict, 39: f_dict, 12: f_dict, 1: f_dict}
h = random.choice(list(g_dict.values()))
i = random.choice(list(h.values()))
j = random.choice(list(i.values()))
k = j + '.'
l = ''
for _ in range(2):
    m = ''
    for _ in range(5):
        n = ''
        for _ in range(5):
            n += m
            m += l
        l += k
o_list = [n for _ in range(10)]
p_list = [o_list for _ in range(5)]
q = random.choice(p_list)
r = random.choice(q)
s = ''
for _ in range(5):
    t = ''
    for _ in range(3):
        t += s
        s += r
u = t + '.'
def v():
    return u
w = v()
x_set = {w, w}
x = random.choice(list(x_set))
y = ''
for _ in range(8):
        if _ == 1:
            continue
        y += x
z = ''
for _ in range(5):
    aa = ''
    for _ in range(2):
        aa += z
        z += y
ab = f'string {aa}'
ac = (ab, ab, ab)
ad, ae, af = ac
ag = ad + ae + af
ah = ''
for _ in range(4):
    for __ in range(2):
                ah += ag
ai_set = {ah, ah, ah, ah}
ai = random.choice(list(ai_set))
aj = ai + '.'
ak_list = [aj for _ in range(8)]
al = random.choice(ak_list)
print(al)