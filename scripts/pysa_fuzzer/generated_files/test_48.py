import random
import math

a = input()
b = a + '.'
c = [b for _ in range(8)]
random.shuffle(c)
d = random.choice(c)
def e():
    return d
def f():
    return e()
g = f()
h = ''
for _ in range(9):
        if _ == 1:
            continue
        h += g
i_dict = {77: h, 8: h, 17: h, 31: h, 50: h}
j_dict = {54: i_dict, 41: i_dict, 48: i_dict, 81: i_dict, 62: i_dict, 66: i_dict, 25: i_dict}
k = random.choice(list(j_dict.values()))
l = random.choice(list(k.values()))
m = ''
for _ in range(5):
    n = ''
    for _ in range(5):
        n += m
        m += l
o = ''
for _ in range(6):
        if _ == 5:
            break
        o += n
p_list = [o for _ in range(2)]
q_list = [p_list for _ in range(10)]
r = random.choice(q_list)
s = random.choice(r)
def t():
    return s
def u():
    return t()
v = u()
w = ''
for _ in range(3):
    w += v
x = ''
for _ in range(3):
    x += w
y = ''
for _ in range(4):
    for __ in range(5):
                y += x
z = f'string {y}'
aa = z[0:]
ab = ''
for _ in range(5):
    ab += aa
ac = ''
counterac = 0
while counterac < 5:
    ad = ''
    counterad = 0
    while counterad < 5:
        ae = ''
        counterae = 0
        while counterae < 2:
            ae += ad
            counterae += 1
            ad += ac
            counterad += 1
        ac += ab
        counterac += 1
af = (ae, ae, ae)
ag, ah, ai = af
aj = ag + ah + ai
ak = f'string {aj}'
print(ak)