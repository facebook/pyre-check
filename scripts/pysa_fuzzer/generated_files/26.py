import random
import math
a = input()
b = a + '.'
c_set = {b, b, b, b, b, b}
c = random.choice(list(c_set))
d = c + '.'
e = ''
for _ in range(2):
    f = ''
    for _ in range(5):
        f += e
        e += d
g = (f, f, f)
h, i, j = g
k = h + i + j
l_list = [k for _ in range(10)]
m_list = [l_list for _ in range(3)]
n_list = [m_list for _ in range(7)]
o = random.choice(n_list)
p = random.choice(o)
q = random.choice(p)
r = [q for _ in range(5)]
random.shuffle(r)
s = random.choice(r)
t = ''
for _ in range(9):
        if _ == 2:
            break
        t += s
u_dict = {92: t, 95: t}
v = random.choice(list(u_dict.values()))
w = f'string {v}'
x = [w for _ in range(8)]
random.shuffle(x)
y = random.choice(x)
z = ''
for _ in range(10):
        if _ == 4:
            break
        z += y
aa = f'string {z}'
ab_list = [aa for _ in range(2)]
ac_list = [ab_list for _ in range(4)]
ad_list = [ac_list for _ in range(4)]
ae = random.choice(ad_list)
af = random.choice(ae)
ag = random.choice(af)
ah_dict = {89: ag, 49: ag}
ai_dict = {46: ah_dict, 57: ah_dict, 82: ah_dict, 50: ah_dict, 11: ah_dict, 90: ah_dict}
aj = random.choice(list(ai_dict.values()))
ak = random.choice(list(aj.values()))
al = f'string {ak}'
am = al[0:]
an = ''
for _ in range(2):
    for __ in range(3):
                an += am
print(an)