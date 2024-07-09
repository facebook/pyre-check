import random
import math

a = input()
b = a[0:]
c = b + '3'
d = ''
for _ in range(6):
        if _ == 1:
            break
        d += c
e = d + '8'
f = e + '9'
g_dict = {82: f, 63: f}
h_dict = {27: g_dict, 84: g_dict, 25: g_dict, 10: g_dict, 64: g_dict, 71: g_dict, 6: g_dict}
i = random.choice(list(h_dict.values()))
j = random.choice(list(i.values()))
if j == j:
    m = j + 'c1'
elif j == '14':
    m = k + 'c2'
else:
    m = l + 'c3'
n_list = [m for _ in range(3)]
o_list = [n_list for _ in range(2)]
p = random.choice(o_list)
q = random.choice(p)
r = ''
for _ in range(5):
    for __ in range(4):
                r += q
s_dict = {95: r, 15: r, 8: r, 83: r, 98: r, 41: r, 42: r, 51: r, 85: r}
t = random.choice(list(s_dict.values()))
u_list = [t for _ in range(7)]
v = random.choice(u_list)
w_list = [v for _ in range(2)]
x = random.choice(w_list)
y = x[0:]
z = ''
for _ in range(4):
    aa = ''
    for _ in range(2):
        ab = ''
        for _ in range(5):
            ab += aa
            aa += z
        z += y
ac_dict = {25: ab, 13: ab, 75: ab, 91: ab, 57: ab, 40: ab}
ad = random.choice(list(ac_dict.values()))
ae = ''
for _ in range(5):
    for __ in range(5):
                ae += ad
af = ''
counteraf = 0
while counteraf < 3:
    ag = ''
    counterag = 0
    while counterag < 2:
        ah = ''
        counterah = 0
        while counterah < 5:
            ah += ag
            counterah += 1
            ag += af
            counterag += 1
        af += ae
        counteraf += 1
ai = [ah for _ in range(7)]
random.shuffle(ai)
aj = random.choice(ai)
ak = aj + '.'
print(ak)