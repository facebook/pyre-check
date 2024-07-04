import random
import math
a = input()
b_dict = {72: a, 39: a, 34: a, 87: a}
c_dict = {6: b_dict, 29: b_dict, 41: b_dict, 86: b_dict, 78: b_dict, 75: b_dict, 63: b_dict}
d_dict = {92: c_dict, 51: c_dict, 49: c_dict, 62: c_dict, 79: c_dict, 29: c_dict, 61: c_dict, 10: c_dict, 78: c_dict, 31: c_dict}
e = random.choice(list(d_dict.values()))
f = random.choice(list(e.values()))
g = random.choice(list(f.values()))
def h():
    return g
i = h()
j = ''
for _ in range(7):
        if _ == 2:
            continue
        j += i
k = ''
for _ in range(5):
        if _ == 5:
            continue
        k += j
l = [k for _ in range(6)]
random.shuffle(l)
m = random.choice(l)
n_set = {m, m, m}
n = random.choice(list(n_set))
o_list = [n for _ in range(8)]
p_list = [o_list for _ in range(4)]
q = random.choice(p_list)
r = random.choice(q)
s = ''
for _ in range(8):
        if _ == 1:
            continue
        s += r
t = s + '6'
u = (t, t, t)
v, w, x = u
y = v + w + x
z_dict = {9: y, 12: y, 68: y, 97: y}
aa = random.choice(list(z_dict.values()))
ab = aa + '.'
def ac():
    return ab
ad = ac()
ae = ''
counterae = 0
while counterae < 3:
    af = ''
    counteraf = 0
    while counteraf < 2:
        af += ae
        counteraf += 1
        ae += ad
        counterae += 1
ag = af[0:]
if ag == '1':
    ah = ag + ' c1'
elif ag == '14':
    ah = ag + ' c2'
else:
    ah = ag + ' c3'
ai = [ah for _ in range(9)]
random.shuffle(ai)
aj = random.choice(ai)
ak_set = {aj, aj, aj, aj, aj, aj, aj}
ak = random.choice(list(ak_set))
print(ak)