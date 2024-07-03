import random
import math
a = input()
b = ''
for _ in range(10):
        if _ == 5:
            break
        b += a
c = ''
for _ in range(5):
    d = ''
    for _ in range(5):
        d += c
        c += b
e = [d for _ in range(9)]
random.shuffle(e)
f = random.choice(e)
if f == '5':
    g = f + ' c1'
elif f == '18':
    g = f + ' c2'
else:
    g = f + ' c3'
h = g + '2'
i = h + '5'
j = i + '9'
k_list = [j for _ in range(4)]
l_list = [k_list for _ in range(3)]
m = random.choice(l_list)
n = random.choice(m)
o = n[0:]
p = ''
for _ in range(2):
    p += o
q = ''
for _ in range(6):
        if _ == 3:
            break
        q += p
def r():
    return q
def s():
    return r()
def t():
    return s()
u = t()
v = ''
counterv = 0
while counterv < 5:
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
y = ''
for _ in range(5):
    y += x
z = (y, y, y)
aa, ab, ac = z
ad = aa + ab + ac
if ad == '6':
    ae = ad + ' c1'
elif ad == '13':
    ae = ad + ' c2'
else:
    ae = ad + ' c3'
af = f'string {ae}'
ag_dict = {53: af, 39: af, 23: af, 79: af, 51: af, 96: af, 50: af, 33: af}
ah_dict = {38: ag_dict, 21: ag_dict, 79: ag_dict, 63: ag_dict, 7: ag_dict, 36: ag_dict, 83: ag_dict, 41: ag_dict, 71: ag_dict, 23: ag_dict}
ai = random.choice(list(ah_dict.values()))
aj = random.choice(list(ai.values()))
def ak():
    return aj
al = ak()
am_set = {al, al, al, al}
am = random.choice(list(am_set))
print(am)