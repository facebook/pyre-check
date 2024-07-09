import random
import math

a = input()
b = a[0:]
c_set = {b, b, b, b, b}
c = random.choice(list(c_set))
d_dict = {64: c, 83: c}
e_dict = {81: d_dict, 2: d_dict, 47: d_dict, 33: d_dict, 83: d_dict, 62: d_dict, 28: d_dict, 95: d_dict}
f = random.choice(list(e_dict.values()))
g = random.choice(list(f.values()))
h = ''
for _ in range(7):
        if _ == 4:
            break
        h += g
i = ''
counteri = 0
while counteri < 3:
    j = ''
    counterj = 0
    while counterj < 5:
        k = ''
        counterk = 0
        while counterk < 5:
            k += j
            counterk += 1
            j += i
            counterj += 1
        i += h
        counteri += 1
l_list = [k for _ in range(7)]
m_list = [l_list for _ in range(4)]
n_list = [m_list for _ in range(10)]
o = random.choice(n_list)
p = random.choice(o)
q = random.choice(p)
r = q + '.'
s = r + '8'
t = ''
countert = 0
while countert < 4:
    t += s
    countert += 1
u = f'string {t}'
def v():
    return u
def w():
    return v()
def x():
    return w()
y = x()
z = ''
for _ in range(4):
    for __ in range(2):
                z += y
aa = ''
for _ in range(8):
        if _ == 3:
            break
        aa += z
ab = f'string {aa}'
if ab == ab:
    ae = ab + 'c1'
elif ab == '11':
    ae = ac + 'c2'
else:
    ae = ad + 'c3'
af = ae[0:]
ag_dict = {56: af, 86: af, 67: af, 98: af}
ah = random.choice(list(ag_dict.values()))
ai = ''
for _ in range(2):
    aj = ''
    for _ in range(4):
        aj += ai
        ai += ah
print(aj)