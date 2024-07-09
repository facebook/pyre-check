import random
import math

a = input()
b = a[0:]
c = ''
for _ in range(2):
    for __ in range(2):
                c += b
d = ''
for _ in range(7):
        if _ == 1:
            continue
        d += c
e = ''
for _ in range(3):
    f = ''
    for _ in range(4):
        f += e
        e += d
g = ''
counterg = 0
while counterg < 3:
    h = ''
    counterh = 0
    while counterh < 2:
        i = ''
        counteri = 0
        while counteri < 4:
            i += h
            counteri += 1
            h += g
            counterh += 1
        g += f
        counterg += 1
def j():
    return i
k = j()
l = f'string {k}'
m_set = {l, l, l, l}
m = random.choice(list(m_set))
n = m[0:]
o = f'string {n}'
p_dict = {18: o, 89: o, 48: o, 75: o, 79: o, 48: o}
q_dict = {34: p_dict, 85: p_dict, 77: p_dict, 83: p_dict, 82: p_dict, 97: p_dict, 83: p_dict, 41: p_dict, 99: p_dict, 100: p_dict}
r_dict = {37: q_dict, 40: q_dict, 33: q_dict, 72: q_dict}
s = random.choice(list(r_dict.values()))
t = random.choice(list(s.values()))
u = random.choice(list(t.values()))
v_dict = {6: u, 23: u, 55: u, 38: u, 34: u, 4: u, 80: u, 63: u, 83: u}
w_dict = {99: v_dict, 22: v_dict, 57: v_dict, 46: v_dict}
x = random.choice(list(w_dict.values()))
y = random.choice(list(x.values()))
z = f'string {y}'
aa = z[0:]
ab = ''
for _ in range(3):
    for __ in range(2):
                ab += aa
ac_list = [ab for _ in range(9)]
ad_list = [ac_list for _ in range(10)]
ae = random.choice(ad_list)
af = random.choice(ae)
def ag():
    return af
def ah():
    return ag()
def ai():
    return ah()
aj = ai()
ak = f'string {aj}'
print(ak)