import random
import math

a = input()
b = ''
for _ in range(10):
        if _ == 4:
            continue
        b += a
def c():
    return b
d = c()
e = ''
countere = 0
while countere < 3:
    f = ''
    counterf = 0
    while counterf < 3:
        f += e
        counterf += 1
        e += d
        countere += 1
g = ''
for _ in range(8):
        if _ == 2:
            continue
        g += f
h = ''
counterh = 0
while counterh < 2:
    h += g
    counterh += 1
i_dict = {89: h, 48: h, 8: h, 97: h, 76: h}
j_dict = {24: i_dict, 9: i_dict, 8: i_dict, 51: i_dict, 75: i_dict, 12: i_dict}
k_dict = {15: j_dict, 22: j_dict}
l = random.choice(list(k_dict.values()))
m = random.choice(list(l.values()))
n = random.choice(list(m.values()))
o = ''
for _ in range(5):
    for __ in range(4):
                o += n
p = o + '.'
q = p[0:]
r = ''
for _ in range(2):
    r += q
s = (r, r, r)
t, u, v = s
w = t + u + v
x = f'string {w}'
y_set = {x, x, x, x}
y = random.choice(list(y_set))
z_list = [y for _ in range(3)]
aa_list = [z_list for _ in range(4)]
ab = random.choice(aa_list)
ac = random.choice(ab)
def ad():
    return ac
def ae():
    return ad()
af = ae()
def ag():
    return af
def ah():
    return ag()
def ai():
    return ah()
aj = ai()
ak = aj + '.'
al_set = {ak, ak, ak, ak, ak}
al = random.choice(list(al_set))
print(al)