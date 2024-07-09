import random
import math

a = input()
b = a[0:]
c = f'string {b}'
d = c + '.'
e = ''
for _ in range(4):
    f = ''
    for _ in range(3):
        f += e
        e += d
g = f + '5'
def h():
    return g
def i():
    return h()
def j():
    return i()
k = j()
l = ''
for _ in range(5):
    for __ in range(4):
                l += k
m = ''
for _ in range(2):
    n = ''
    for _ in range(2):
        n += m
        m += l
o = ''
for _ in range(2):
    for __ in range(2):
                o += n
p = f'string {o}'
q_list = [p for _ in range(9)]
r_list = [q_list for _ in range(7)]
s = random.choice(r_list)
t = random.choice(s)
u = t + '2'
v = u + '4'
w = v + '6'
x_dict = {68: w, 20: w, 96: w}
y_dict = {75: x_dict, 93: x_dict, 42: x_dict, 54: x_dict}
z = random.choice(list(y_dict.values()))
aa = random.choice(list(z.values()))
ab = aa + '7'
ac = ab + '9'
ad = ac + '5'
ae = ad[0:]
if ae == ae:
    ah = ae + 'c1'
elif ae == '19':
    ah = af + 'c2'
else:
    ah = ag + 'c3'
ai = [ah for _ in range(6)]
random.shuffle(ai)
aj = random.choice(ai)
ak_set = {aj, aj, aj, aj, aj}
ak = random.choice(list(ak_set))
print(ak)