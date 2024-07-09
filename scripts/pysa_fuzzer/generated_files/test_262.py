import random
import math

a = input()
b = ''
for _ in range(2):
    for __ in range(2):
                b += a
c_set = {b, b, b, b, b, b, b, b, b}
c = random.choice(list(c_set))
d = ''
for _ in range(3):
    e = ''
    for _ in range(5):
        e += d
        d += c
def f():
    return e
def g():
    return f()
h = g()
i = f'string {h}'
j = ''
for _ in range(5):
    k = ''
    for _ in range(4):
        k += j
        j += i
l = ''
for _ in range(5):
    for __ in range(4):
                l += k
m_list = [l for _ in range(4)]
n = random.choice(m_list)
o_dict = {51: n, 96: n, 21: n, 49: n, 72: n, 49: n, 41: n}
p_dict = {65: o_dict, 31: o_dict, 37: o_dict}
q_dict = {11: p_dict, 82: p_dict, 20: p_dict, 78: p_dict, 91: p_dict, 26: p_dict, 5: p_dict, 91: p_dict, 16: p_dict}
r = random.choice(list(q_dict.values()))
s = random.choice(list(r.values()))
t = random.choice(list(s.values()))
u = (t, t, t)
v, w, x = u
y = v + w + x
z = y + '.'
aa = f'string {z}'
ab_list = [aa for _ in range(5)]
ac_list = [ab_list for _ in range(8)]
ad_list = [ac_list for _ in range(7)]
ae = random.choice(ad_list)
af = random.choice(ae)
ag = random.choice(af)
def ah():
    return ag
ai = ah()
aj = ''
for _ in range(8):
        if _ == 4:
            continue
        aj += ai
ak = aj[0:]
def al():
    return ak
def am():
    return al()
an = am()
ao = ''
for _ in range(5):
    ap = ''
    for _ in range(5):
        ap += ao
        ao += an
print(ap)