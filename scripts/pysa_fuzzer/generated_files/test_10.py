import random
import math

a = input()
b_set = {a, a, a, a, a, a, a, a, a}
b = random.choice(list(b_set))
c = b + '4'
d = c + '9'
e = d + '4'
f = f'string {e}'
g = ''
for _ in range(5):
        if _ == 2:
            continue
        g += f
h = (g, g, g)
i, j, k = h
l = i + j + k
m = ''
counterm = 0
while counterm < 2:
    n = ''
    countern = 0
    while countern < 3:
        o = ''
        countero = 0
        while countero < 4:
            o += n
            countero += 1
            n += m
            countern += 1
        m += l
        counterm += 1
def p():
    return o
def q():
    return p()
def r():
    return q()
s = r()
t = ''
for _ in range(4):
    for __ in range(4):
                t += s
u = ''
for _ in range(6):
        if _ == 1:
            continue
        u += t
v = ''
counterv = 0
while counterv < 2:
    w = ''
    counterw = 0
    while counterw < 2:
        x = ''
        counterx = 0
        while counterx < 3:
            x += w
            counterx += 1
            w += v
            counterw += 1
        v += u
        counterv += 1
y_dict = {8: x, 76: x, 21: x, 55: x, 46: x, 42: x, 80: x}
z_dict = {78: y_dict, 61: y_dict, 9: y_dict, 80: y_dict, 64: y_dict, 60: y_dict, 22: y_dict, 58: y_dict, 32: y_dict, 21: y_dict}
aa_dict = {56: z_dict, 60: z_dict}
ab = random.choice(list(aa_dict.values()))
ac = random.choice(list(ab.values()))
ad = random.choice(list(ac.values()))
ae_dict = {9: ad, 86: ad, 57: ad, 99: ad, 100: ad, 99: ad}
af = random.choice(list(ae_dict.values()))
ag = af[0:]
ah = f'string {ag}'
def ai():
    return ah
def aj():
    return ai()
ak = aj()
al = [ak for _ in range(6)]
random.shuffle(al)
am = random.choice(al)
an = am + '5'
ao = an + '4'
ap = ao + '7'
aq = ''
for _ in range(4):
    for __ in range(5):
                aq += ap
print(aq)