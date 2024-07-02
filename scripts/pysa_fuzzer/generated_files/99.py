import random
import math
a = input()
b = ''
for _ in range(9):
        if _ == 4:
            continue
        b += a
c = f'string {b}'
d_set = {c, c, c, c}
d = random.choice(list(d_set))
e = ''
countere = 0
while countere < 3:
    e += d
    countere += 1
f = ''
for _ in range(3):
    g = ''
    for _ in range(4):
        g += f
        f += e
h = g + '1'
i = h + '8'
j = ''
for _ in range(7):
        if _ == 1:
            continue
        j += i
k_dict = {8: j, 98: j, 12: j, 44: j}
l_dict = {35: k_dict, 79: k_dict, 24: k_dict, 29: k_dict}
m = random.choice(list(l_dict.values()))
n = random.choice(list(m.values()))
o = n[0:]
p = ''
for _ in range(4):
    p += o
q = ''
for _ in range(3):
    r = ''
    for _ in range(5):
        s = ''
        for _ in range(4):
            s += r
            r += q
        q += p
t = ''
for _ in range(4):
    t += s
def u():
    return t
def v():
    return u()
w = v()
def x():
    return w
y = x()
z = y + '1'
aa = z + '3'
ab = aa + '2'
ac = ab[0:]
def ad():
    return ac
ae = ad()
af_dict = {98: ae, 61: ae, 95: ae, 19: ae, 65: ae, 30: ae, 4: ae}
ag_dict = {70: af_dict, 47: af_dict, 85: af_dict, 76: af_dict, 15: af_dict}
ah_dict = {95: ag_dict, 31: ag_dict, 14: ag_dict, 65: ag_dict, 11: ag_dict, 27: ag_dict, 59: ag_dict}
ai = random.choice(list(ah_dict.values()))
aj = random.choice(list(ai.values()))
ak = random.choice(list(aj.values()))
print(ak)