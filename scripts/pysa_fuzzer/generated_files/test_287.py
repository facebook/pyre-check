import random
import math

a = input()
def b():
    return a
c = b()
d_dict = {16: c, 67: c, 35: c, 89: c, 11: c}
e = random.choice(list(d_dict.values()))
f = e[0:]
g = ''
counterg = 0
while counterg < 4:
    h = ''
    counterh = 0
    while counterh < 5:
        h += g
        counterh += 1
        g += f
        counterg += 1
i_dict = {59: h, 31: h, 73: h, 63: h, 49: h, 75: h, 57: h, 89: h}
j_dict = {19: i_dict, 78: i_dict, 80: i_dict, 6: i_dict, 71: i_dict}
k_dict = {40: j_dict, 54: j_dict}
l = random.choice(list(k_dict.values()))
m = random.choice(list(l.values()))
n = random.choice(list(m.values()))
o = n + '.'
p = (o, o, o)
q, r, s = p
t = q + r + s
u = [t for _ in range(8)]
random.shuffle(u)
v = random.choice(u)
w = ''
for _ in range(2):
    for __ in range(4):
                w += v
x_list = [w for _ in range(10)]
y_list = [x_list for _ in range(6)]
z_list = [y_list for _ in range(8)]
aa = random.choice(z_list)
ab = random.choice(aa)
ac = random.choice(ab)
if ac == ac:
    af = ac + 'c1'
elif ac == '14':
    af = ad + 'c2'
else:
    af = ae + 'c3'
ag = ''
for _ in range(2):
    for __ in range(5):
                ag += af
ah = ag + '.'
def ai():
    return ah
aj = ai()
ak = ''
for _ in range(6):
        if _ == 4:
            break
        ak += aj
al_dict = {20: ak, 97: ak, 51: ak, 54: ak}
am_dict = {41: al_dict, 10: al_dict, 77: al_dict, 32: al_dict, 95: al_dict}
an_dict = {37: am_dict, 22: am_dict, 91: am_dict, 54: am_dict}
ao = random.choice(list(an_dict.values()))
ap = random.choice(list(ao.values()))
aq = random.choice(list(ap.values()))
def ar():
    return aq
at = ar()
au = [at for _ in range(10)]
random.shuffle(au)
av = random.choice(au)
print(av)