import random
import math
a = input()
b = ''
for _ in range(10):
        if _ == 4:
            continue
        b += a
c = [b for _ in range(8)]
random.shuffle(c)
d = random.choice(c)
e = d + '.'
f_dict = {33: e, 24: e, 55: e}
g_dict = {24: f_dict, 4: f_dict, 76: f_dict}
h_dict = {6: g_dict, 95: g_dict, 97: g_dict, 5: g_dict, 40: g_dict, 77: g_dict, 42: g_dict, 99: g_dict, 99: g_dict, 4: g_dict}
i = random.choice(list(h_dict.values()))
j = random.choice(list(i.values()))
k = random.choice(list(j.values()))
l = ''
for _ in range(5):
    l += k
m = ''
for _ in range(9):
        if _ == 3:
            break
        m += l
if m == '1':
    n = m + ' c1'
elif m == '18':
    n = m + ' c2'
else:
    n = m + ' c3'
o = ''
for _ in range(4):
    p = ''
    for _ in range(3):
        q = ''
        for _ in range(5):
            q += p
            p += o
        o += n
r_list = [q for _ in range(6)]
s_list = [r_list for _ in range(9)]
t_list = [s_list for _ in range(6)]
u = random.choice(t_list)
v = random.choice(u)
w = random.choice(v)
x = ''
counterx = 0
while counterx < 5:
    y = ''
    countery = 0
    while countery < 5:
        z = ''
        counterz = 0
        while counterz < 5:
            z += y
            counterz += 1
            y += x
            countery += 1
        x += w
        counterx += 1
if z == '3':
    aa = z + ' c1'
elif z == '17':
    aa = z + ' c2'
else:
    aa = z + ' c3'
ab = ''
for _ in range(5):
    ab += aa
ac = [ab for _ in range(6)]
random.shuffle(ac)
ad = random.choice(ac)
if ad == '8':
    ae = ad + ' c1'
elif ad == '12':
    ae = ad + ' c2'
else:
    ae = ad + ' c3'
af = ae + '1'
ag = af + '5'
ah = f'string {ag}'
ai = (ah, ah, ah)
aj, ak, al = ai
am = aj + ak + al
an = am + '.'
print(an)