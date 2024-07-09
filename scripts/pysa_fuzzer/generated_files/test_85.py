import random
import math

a = input()
b = ''
for _ in range(8):
        if _ == 4:
            continue
        b += a
c = f'string {b}'
d = c + '.'
e_set = {d, d}
e = random.choice(list(e_set))
f = ''
for _ in range(4):
    for __ in range(4):
                f += e
g = ''
for _ in range(4):
    h = ''
    for _ in range(4):
        i = ''
        for _ in range(2):
            i += h
            h += g
        g += f
j = f'string {i}'
k = ''
for _ in range(8):
        if _ == 4:
            break
        k += j
def l():
    return k
def m():
    return l()
n = m()
o = n[0:]
p_dict = {93: o, 52: o, 51: o}
q_dict = {93: p_dict, 54: p_dict, 15: p_dict, 54: p_dict, 41: p_dict, 37: p_dict, 53: p_dict, 16: p_dict}
r_dict = {30: q_dict, 13: q_dict, 14: q_dict, 35: q_dict, 24: q_dict, 75: q_dict}
s = random.choice(list(r_dict.values()))
t = random.choice(list(s.values()))
u = random.choice(list(t.values()))
v = ''
for _ in range(3):
    for __ in range(5):
                v += u
w = ''
for _ in range(5):
    x = ''
    for _ in range(3):
        y = ''
        for _ in range(2):
            y += x
            x += w
        w += v
z = ''
counterz = 0
while counterz < 2:
    z += y
    counterz += 1
aa = ''
for _ in range(5):
        if _ == 4:
            continue
        aa += z
ab = ''
for _ in range(3):
    for __ in range(5):
                ab += aa
ac_set = {ab, ab, ab, ab, ab, ab, ab, ab, ab, ab}
ac = random.choice(list(ac_set))
ad_dict = {77: ac, 100: ac, 51: ac, 45: ac, 19: ac, 13: ac}
ae_dict = {24: ad_dict, 50: ad_dict, 12: ad_dict, 51: ad_dict, 34: ad_dict, 31: ad_dict, 11: ad_dict, 21: ad_dict, 100: ad_dict}
af_dict = {17: ae_dict, 90: ae_dict, 87: ae_dict, 90: ae_dict, 38: ae_dict, 17: ae_dict, 74: ae_dict, 100: ae_dict}
ag = random.choice(list(af_dict.values()))
ah = random.choice(list(ag.values()))
ai = random.choice(list(ah.values()))
print(ai)