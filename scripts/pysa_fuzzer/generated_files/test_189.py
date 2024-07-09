import random
import math

a = input()
b_set = {a, a, a, a}
b = random.choice(list(b_set))
def c():
    return b
def d():
    return c()
def e():
    return d()
f = e()
g = f[0:]
h = ''
counterh = 0
while counterh < 2:
    i = ''
    counteri = 0
    while counteri < 4:
        i += h
        counteri += 1
        h += g
        counterh += 1
def j():
    return i
def k():
    return j()
def l():
    return k()
m = l()
n_dict = {66: m, 9: m, 57: m, 47: m, 10: m, 52: m, 50: m, 37: m}
o = random.choice(list(n_dict.values()))
p = [o for _ in range(6)]
random.shuffle(p)
q = random.choice(p)
r_list = [q for _ in range(8)]
s_list = [r_list for _ in range(5)]
t_list = [s_list for _ in range(7)]
u = random.choice(t_list)
v = random.choice(u)
w = random.choice(v)
x_dict = {35: w, 59: w, 62: w, 52: w}
y_dict = {85: x_dict, 81: x_dict, 45: x_dict, 40: x_dict, 18: x_dict, 37: x_dict, 35: x_dict, 70: x_dict, 77: x_dict, 80: x_dict}
z = random.choice(list(y_dict.values()))
aa = random.choice(list(z.values()))
ab = ''
for _ in range(4):
    for __ in range(3):
                ab += aa
ac = ''
counterac = 0
while counterac < 5:
    ac += ab
    counterac += 1
ad = ac + '4'
ae = ad + '3'
af = f'string {ae}'
if af == af:
    ai = af + 'c1'
elif af == '14':
    ai = ag + 'c2'
else:
    ai = ah + 'c3'
aj = ''
for _ in range(4):
    for __ in range(2):
                aj += ai
ak = f'string {aj}'
al_set = {ak, ak, ak, ak, ak, ak, ak, ak, ak}
al = random.choice(list(al_set))
am = al[0:]
print(am)