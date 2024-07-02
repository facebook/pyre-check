import random
import math
a = input()
b = ''
for _ in range(5):
        if _ == 2:
            break
        b += a
c_dict = {43: b, 76: b, 44: b, 43: b, 11: b, 80: b, 15: b}
d_dict = {84: c_dict, 32: c_dict, 75: c_dict, 75: c_dict, 15: c_dict, 72: c_dict, 68: c_dict, 17: c_dict}
e = random.choice(list(d_dict.values()))
f = random.choice(list(e.values()))
g = ''
for _ in range(5):
    for __ in range(5):
                g += f
h = f'string {g}'
i = f'string {h}'
if i == '6':
    j = i + ' c1'
elif i == '20':
    j = i + ' c2'
else:
    j = i + ' c3'
k_list = [j for _ in range(10)]
l_list = [k_list for _ in range(7)]
m = random.choice(l_list)
n = random.choice(m)
o = ''
for _ in range(5):
        if _ == 2:
            break
        o += n
def p():
    return o
q = p()
if q == '5':
    r = q + ' c1'
elif q == '15':
    r = q + ' c2'
else:
    r = q + ' c3'
s = r + '5'
t = s + '9'
u = t + '7'
v = ''
for _ in range(7):
        if _ == 2:
            continue
        v += u
w = v + '.'
x = [w for _ in range(10)]
random.shuffle(x)
y = random.choice(x)
z = ''
for _ in range(4):
    for __ in range(2):
                z += y
aa = z + '.'
ab = [aa for _ in range(6)]
random.shuffle(ab)
ac = random.choice(ab)
if ac == '4':
    ad = ac + ' c1'
elif ac == '18':
    ad = ac + ' c2'
else:
    ad = ac + ' c3'
print(ad)