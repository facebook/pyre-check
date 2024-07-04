import random
import math
a = input()
b = ''
counterb = 0
while counterb < 4:
    c = ''
    counterc = 0
    while counterc < 5:
        c += b
        counterc += 1
        b += a
        counterb += 1
d_dict = {2: c, 1: c, 74: c, 18: c, 60: c, 43: c, 87: c, 10: c, 3: c, 12: c}
e_dict = {29: d_dict, 11: d_dict, 68: d_dict, 11: d_dict, 1: d_dict, 50: d_dict, 60: d_dict, 53: d_dict}
f = random.choice(list(e_dict.values()))
g = random.choice(list(f.values()))
h = ''
for _ in range(3):
    i = ''
    for _ in range(2):
        j = ''
        for _ in range(4):
            j += i
            i += h
        h += g
if j == '6':
    k = j + ' c1'
elif j == '12':
    k = j + ' c2'
else:
    k = j + ' c3'
l = ''
for _ in range(3):
    l += k
m = ''
for _ in range(2):
    for __ in range(5):
                m += l
def n():
    return m
def o():
    return n()
p = o()
q = [p for _ in range(10)]
random.shuffle(q)
r = random.choice(q)
s = ''
for _ in range(10):
        if _ == 3:
            break
        s += r
t = s + '.'
u = f'string {t}'
v = u + '6'
w = v + '9'
x = [w for _ in range(9)]
random.shuffle(x)
y = random.choice(x)
z = y + '.'
aa = z[0:]
ab = ''
counterab = 0
while counterab < 4:
    ac = ''
    counterac = 0
    while counterac < 2:
        ad = ''
        counterad = 0
        while counterad < 3:
            ad += ac
            counterad += 1
            ac += ab
            counterac += 1
        ab += aa
        counterab += 1
ae = (ad, ad, ad)
af, ag, ah = ae
ai = af + ag + ah
def aj():
    return ai
def ak():
    return aj()
al = ak()
print(al)