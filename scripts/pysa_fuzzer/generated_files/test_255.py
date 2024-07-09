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
f_dict = {70: e, 91: e, 91: e, 13: e, 82: e, 68: e, 89: e, 63: e, 60: e, 100: e}
g_dict = {63: f_dict, 58: f_dict, 32: f_dict, 78: f_dict, 10: f_dict, 16: f_dict, 56: f_dict}
h_dict = {43: g_dict, 18: g_dict, 66: g_dict, 85: g_dict, 94: g_dict, 81: g_dict, 97: g_dict}
i = random.choice(list(h_dict.values()))
j = random.choice(list(i.values()))
k = random.choice(list(j.values()))
l_list = [k for _ in range(8)]
m_list = [l_list for _ in range(5)]
n_list = [m_list for _ in range(3)]
o = random.choice(n_list)
p = random.choice(o)
q = random.choice(p)
r = f'string {q}'
s = ''
for _ in range(4):
    for __ in range(3):
                s += r
t = s + '3'
u = t + '7'
v = u + '1'
def w():
    return v
x = w()
y_list = [x for _ in range(9)]
z = random.choice(y_list)
aa_set = {z, z, z}
aa = random.choice(list(aa_set))
ab = f'string {aa}'
ac = ''
counterac = 0
while counterac < 4:
    ad = ''
    counterad = 0
    while counterad < 2:
        ad += ac
        counterad += 1
        ac += ab
        counterac += 1
ae = ''
for _ in range(4):
    for __ in range(5):
                ae += ad
af = ''
for _ in range(5):
    af += ae
ag = ''
for _ in range(4):
    for __ in range(4):
                ag += af
ah = ''
counterah = 0
while counterah < 5:
    ah += ag
    counterah += 1
ai_set = {ah, ah, ah, ah, ah}
ai = random.choice(list(ai_set))
aj = ai[0:]
ak = aj + '9'
print(ak)