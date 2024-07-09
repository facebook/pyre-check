import random
import math

a = input()
b = ''
for _ in range(5):
    b += a
c = ''
for _ in range(4):
    c += b
d_set = {c, c, c, c, c, c, c, c, c, c}
d = random.choice(list(d_set))
e_list = [d for _ in range(4)]
f_list = [e_list for _ in range(7)]
g = random.choice(f_list)
h = random.choice(g)
i = h + '7'
j = [i for _ in range(7)]
random.shuffle(j)
k = random.choice(j)
def l():
    return k
def m():
    return l()
def n():
    return m()
o = n()
p_list = [o for _ in range(5)]
q_list = [p_list for _ in range(8)]
r = random.choice(q_list)
s = random.choice(r)
def t():
    return s
def u():
    return t()
def v():
    return u()
w = v()
x = ''
for _ in range(5):
        if _ == 2:
            continue
        x += w
y = [x for _ in range(7)]
random.shuffle(y)
z = random.choice(y)
aa = z + '.'
ab = ''
counterab = 0
while counterab < 3:
    ab += aa
    counterab += 1
if ab == ab:
    ae = ab + 'c1'
elif ab == '20':
    ae = ac + 'c2'
else:
    ae = ad + 'c3'
af = ''
counteraf = 0
while counteraf < 3:
    ag = ''
    counterag = 0
    while counterag < 4:
        ag += af
        counterag += 1
        af += ae
        counteraf += 1
ah_dict = {95: ag, 26: ag, 67: ag, 68: ag, 12: ag, 55: ag, 1: ag, 39: ag}
ai_dict = {58: ah_dict, 19: ah_dict, 87: ah_dict, 1: ah_dict, 30: ah_dict}
aj = random.choice(list(ai_dict.values()))
ak = random.choice(list(aj.values()))
def al():
    return ak
def am():
    return al()
def an():
    return am()
ao = an()
ap = ''
for _ in range(4):
    for __ in range(4):
                ap += ao
print(ap)