import random
import math

a = input()
b = ''
counterb = 0
while counterb < 5:
    b += a
    counterb += 1
c = ''
counterc = 0
while counterc < 3:
    d = ''
    counterd = 0
    while counterd < 3:
        e = ''
        countere = 0
        while countere < 5:
            e += d
            countere += 1
            d += c
            counterd += 1
        c += b
        counterc += 1
f_dict = {21: e, 45: e, 81: e, 98: e, 70: e, 1: e, 23: e}
g_dict = {100: f_dict, 54: f_dict, 87: f_dict, 1: f_dict, 85: f_dict}
h = random.choice(list(g_dict.values()))
i = random.choice(list(h.values()))
j = ''
for _ in range(7):
        if _ == 2:
            break
        j += i
k = j[0:]
l = ''
for _ in range(9):
        if _ == 3:
            break
        l += k
m_list = [l for _ in range(10)]
n = random.choice(m_list)
o = n + '.'
p_dict = {33: o, 57: o, 21: o}
q_dict = {66: p_dict, 9: p_dict, 22: p_dict, 45: p_dict, 46: p_dict, 28: p_dict, 74: p_dict, 1: p_dict, 50: p_dict}
r = random.choice(list(q_dict.values()))
s = random.choice(list(r.values()))
t = s[0:]
u = (t, t, t)
v, w, x = u
y = v + w + x
z = ''
for _ in range(2):
    z += y
aa = z[0:]
ab = aa + '4'
ac = f'string {ab}'
ad_dict = {56: ac, 55: ac, 56: ac, 30: ac, 69: ac, 66: ac, 9: ac, 74: ac, 58: ac}
ae = random.choice(list(ad_dict.values()))
def af():
    return ae
def ag():
    return af()
ah = ag()
ai = (ah, ah, ah)
aj, ak, al = ai
am = aj + ak + al
print(am)