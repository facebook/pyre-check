import random
import math
a = input()
b = a[0:]
c = ''
counterc = 0
while counterc < 3:
    d = ''
    counterd = 0
    while counterd < 2:
        d += c
        counterd += 1
        c += b
        counterc += 1
e = d[0:]
f = ''
for _ in range(5):
        if _ == 2:
            continue
        f += e
if f == '2':
    g = f + ' c1'
elif f == '15':
    g = f + ' c2'
else:
    g = f + ' c3'
h = f'string {g}'
i = f'string {h}'
j_set = {i, i, i, i, i, i}
j = random.choice(list(j_set))
k_list = [j for _ in range(7)]
l_list = [k_list for _ in range(5)]
m_list = [l_list for _ in range(5)]
n = random.choice(m_list)
o = random.choice(n)
p = random.choice(o)
q = ''
for _ in range(5):
        if _ == 2:
            break
        q += p
r_dict = {13: q, 83: q, 28: q}
s_dict = {94: r_dict, 97: r_dict, 59: r_dict, 6: r_dict, 28: r_dict, 65: r_dict, 21: r_dict, 75: r_dict}
t_dict = {80: s_dict, 67: s_dict}
u = random.choice(list(t_dict.values()))
v = random.choice(list(u.values()))
w = random.choice(list(v.values()))
def x():
    return w
def y():
    return x()
z = y()
aa = z + '.'
ab = aa + '8'
ac = ab + '2'
ad = ac + '6'
ae = ad + '7'
af = ''
counteraf = 0
while counteraf < 3:
    ag = ''
    counterag = 0
    while counterag < 3:
        ag += af
        counterag += 1
        af += ae
        counteraf += 1
ah = ''
for _ in range(5):
    ah += ag
ai = ''
for _ in range(6):
        if _ == 5:
            continue
        ai += ah
print(ai)