import random
import math

a = input()
b = a[0:]
c = ''
for _ in range(2):
    c += b
d_list = [c for _ in range(3)]
e = random.choice(d_list)
f = (e, e, e)
g, h, i = f
j = g + h + i
k_set = {j, j}
k = random.choice(list(k_set))
def l():
    return k
m = l()
n = m + '.'
o_dict = {34: n, 79: n, 100: n, 41: n, 39: n}
p_dict = {16: o_dict, 13: o_dict, 79: o_dict, 8: o_dict, 79: o_dict, 30: o_dict, 72: o_dict, 91: o_dict, 35: o_dict, 2: o_dict}
q_dict = {98: p_dict, 20: p_dict, 43: p_dict, 55: p_dict, 26: p_dict, 44: p_dict, 56: p_dict, 30: p_dict}
r = random.choice(list(q_dict.values()))
s = random.choice(list(r.values()))
t = random.choice(list(s.values()))
u = t[0:]
v_set = {u, u}
v = random.choice(list(v_set))
w = ''
for _ in range(4):
    for __ in range(3):
                w += v
x = w[0:]
y = f'string {x}'
z_list = [y for _ in range(6)]
aa = random.choice(z_list)
ab = aa + '.'
ac = ''
for _ in range(4):
    ad = ''
    for _ in range(3):
        ae = ''
        for _ in range(5):
            ae += ad
            ad += ac
        ac += ab
af = f'string {ae}'
ag = ''
for _ in range(4):
    ah = ''
    for _ in range(3):
        ai = ''
        for _ in range(3):
            ai += ah
            ah += ag
        ag += af
print(ai)