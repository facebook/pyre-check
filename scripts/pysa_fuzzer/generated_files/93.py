import random
import math
a = input()
def b():
    return a
def c():
    return b()
def d():
    return c()
e = d()
f = ''
for _ in range(7):
        if _ == 3:
            continue
        f += e
g_dict = {77: f, 69: f, 72: f, 62: f, 1: f}
h_dict = {83: g_dict, 23: g_dict}
i_dict = {1: h_dict, 61: h_dict, 20: h_dict, 15: h_dict, 72: h_dict, 4: h_dict, 48: h_dict, 55: h_dict, 17: h_dict, 65: h_dict}
j = random.choice(list(i_dict.values()))
k = random.choice(list(j.values()))
l = random.choice(list(k.values()))
m = f'string {l}'
n_dict = {57: m, 32: m}
o_dict = {53: n_dict, 100: n_dict, 46: n_dict, 71: n_dict, 2: n_dict, 35: n_dict, 20: n_dict, 71: n_dict}
p = random.choice(list(o_dict.values()))
q = random.choice(list(p.values()))
r = q + '.'
s_set = {r, r, r, r, r, r, r, r, r}
s = random.choice(list(s_set))
t = ''
for _ in range(3):
    for __ in range(3):
                t += s
u = [t for _ in range(9)]
random.shuffle(u)
v = random.choice(u)
w = f'string {v}'
x = f'string {w}'
y_set = {x, x, x, x, x}
y = random.choice(list(y_set))
z_list = [y for _ in range(7)]
aa_list = [z_list for _ in range(9)]
ab = random.choice(aa_list)
ac = random.choice(ab)
ad = ac[0:]
ae = ''
for _ in range(6):
        if _ == 1:
            break
        ae += ad
af = f'string {ae}'
ag = ''
counterag = 0
while counterag < 4:
    ah = ''
    counterah = 0
    while counterah < 2:
        ah += ag
        counterah += 1
        ag += af
        counterag += 1
ai = ''
for _ in range(3):
    aj = ''
    for _ in range(5):
        ak = ''
        for _ in range(2):
            ak += aj
            aj += ai
        ai += ah
print(ak)