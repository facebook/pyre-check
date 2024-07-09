import random
import math

a = input()
b = ''
for _ in range(7):
        if _ == 3:
            continue
        b += a
c = b + '.'
d_dict = {41: c, 18: c, 20: c, 25: c, 29: c, 62: c, 98: c, 40: c}
e_dict = {44: d_dict, 23: d_dict, 71: d_dict, 67: d_dict, 17: d_dict, 3: d_dict, 59: d_dict}
f = random.choice(list(e_dict.values()))
g = random.choice(list(f.values()))
h = g + '.'
i = ''
for _ in range(3):
    for __ in range(2):
                i += h
j = i[0:]
def k():
    return j
def l():
    return k()
m = l()
n = ''
for _ in range(4):
    for __ in range(4):
                n += m
o_dict = {87: n, 90: n, 100: n}
p_dict = {98: o_dict, 92: o_dict, 55: o_dict, 64: o_dict, 99: o_dict, 28: o_dict}
q = random.choice(list(p_dict.values()))
r = random.choice(list(q.values()))
s = r[0:]
t = ''
for _ in range(8):
        if _ == 1:
            break
        t += s
if t == t:
    w = t + 'c1'
elif t == '14':
    w = u + 'c2'
else:
    w = v + 'c3'
x_list = [w for _ in range(5)]
y_list = [x_list for _ in range(2)]
z_list = [y_list for _ in range(3)]
aa = random.choice(z_list)
ab = random.choice(aa)
ac = random.choice(ab)
ad = ''
for _ in range(9):
        if _ == 2:
            continue
        ad += ac
ae = ad[0:]
af = ''
for _ in range(9):
        if _ == 1:
            continue
        af += ae
ag = ''
for _ in range(10):
        if _ == 5:
            continue
        ag += af
ah = ''
counterah = 0
while counterah < 4:
    ah += ag
    counterah += 1
print(ah)