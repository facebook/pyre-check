import random
import math
a = input()
b = ''
for _ in range(8):
        if _ == 4:
            continue
        b += a
c_set = {b, b, b, b, b, b, b, b}
c = random.choice(list(c_set))
if c == '4':
    d = c + ' c1'
elif c == '14':
    d = c + ' c2'
else:
    d = c + ' c3'
e = ''
for _ in range(5):
    for __ in range(3):
                e += d
f = f'string {e}'
g_list = [f for _ in range(5)]
h = random.choice(g_list)
i_list = [h for _ in range(7)]
j_list = [i_list for _ in range(7)]
k = random.choice(j_list)
l = random.choice(k)
def m():
    return l
def n():
    return m()
def o():
    return n()
p = o()
q_dict = {43: p, 38: p, 92: p, 95: p, 47: p, 96: p}
r_dict = {87: q_dict, 56: q_dict, 81: q_dict, 89: q_dict, 62: q_dict}
s = random.choice(list(r_dict.values()))
t = random.choice(list(s.values()))
u_list = [t for _ in range(7)]
v_list = [u_list for _ in range(9)]
w = random.choice(v_list)
x = random.choice(w)
y_set = {x, x}
y = random.choice(list(y_set))
z = ''
counterz = 0
while counterz < 2:
    z += y
    counterz += 1
aa = ''
for _ in range(2):
    for __ in range(2):
                aa += z
ab_dict = {28: aa, 35: aa, 74: aa, 64: aa, 70: aa, 2: aa, 14: aa}
ac = random.choice(list(ab_dict.values()))
ad = (ac, ac, ac)
ae, af, ag = ad
ah = ae + af + ag
ai_set = {ah, ah, ah, ah, ah, ah, ah, ah, ah}
ai = random.choice(list(ai_set))
def aj():
    return ai
def ak():
    return aj()
al = ak()
am = ''
for _ in range(4):
    am += al
print(am)