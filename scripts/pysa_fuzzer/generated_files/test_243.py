import random
import math

a = input()
b = ''
for _ in range(10):
        if _ == 2:
            continue
        b += a
c_dict = {80: b, 85: b, 82: b, 98: b, 15: b, 83: b}
d = random.choice(list(c_dict.values()))
e = ''
countere = 0
while countere < 5:
    f = ''
    counterf = 0
    while counterf < 5:
        f += e
        counterf += 1
        e += d
        countere += 1
g = f[0:]
h_dict = {9: g, 98: g, 69: g, 79: g, 56: g, 70: g, 22: g, 21: g, 10: g}
i_dict = {24: h_dict, 88: h_dict, 31: h_dict, 49: h_dict, 36: h_dict, 82: h_dict, 30: h_dict, 67: h_dict, 74: h_dict}
j_dict = {43: i_dict, 58: i_dict, 71: i_dict, 17: i_dict, 34: i_dict, 67: i_dict, 5: i_dict, 35: i_dict}
k = random.choice(list(j_dict.values()))
l = random.choice(list(k.values()))
m = random.choice(list(l.values()))
n_set = {m, m}
n = random.choice(list(n_set))
o = ''
for _ in range(10):
        if _ == 4:
            break
        o += n
p = ''
for _ in range(3):
    q = ''
    for _ in range(2):
        r = ''
        for _ in range(2):
            r += q
            q += p
        p += o
s = ''
counters = 0
while counters < 5:
    s += r
    counters += 1
def t():
    return s
def u():
    return t()
def v():
    return u()
w = v()
def x():
    return w
y = x()
z_set = {y, y, y, y, y, y, y}
z = random.choice(list(z_set))
aa = z + '.'
ab_dict = {64: aa, 74: aa, 88: aa}
ac = random.choice(list(ab_dict.values()))
def ad():
    return ac
def ae():
    return ad()
af = ae()
if af == af:
    ai = af + 'c1'
elif af == '18':
    ai = ag + 'c2'
else:
    ai = ah + 'c3'
aj_set = {ai, ai, ai, ai, ai, ai, ai, ai}
aj = random.choice(list(aj_set))
ak_list = [aj for _ in range(4)]
al = random.choice(ak_list)
print(al)