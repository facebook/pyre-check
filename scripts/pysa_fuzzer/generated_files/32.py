import random
import math
a = input()
b = a[0:]
c_set = {b, b, b, b, b}
c = random.choice(list(c_set))
d = ''
counterd = 0
while counterd < 4:
    e = ''
    countere = 0
    while countere < 5:
        f = ''
        counterf = 0
        while counterf < 3:
            f += e
            counterf += 1
            e += d
            countere += 1
        d += c
        counterd += 1
g = f'string {f}'
h_dict = {33: g, 9: g, 50: g, 32: g, 51: g, 2: g, 18: g, 6: g, 91: g, 81: g}
i_dict = {53: h_dict, 82: h_dict, 16: h_dict, 74: h_dict}
j = random.choice(list(i_dict.values()))
k = random.choice(list(j.values()))
l = f'string {k}'
m = ''
counterm = 0
while counterm < 5:
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
p = ''
for _ in range(4):
    p += o
q = (p, p, p)
r, s, t = q
u = r + s + t
v_set = {u, u, u, u}
v = random.choice(list(v_set))
w = [v for _ in range(6)]
random.shuffle(w)
x = random.choice(w)
y = x[0:]
if y == '3':
    z = y + ' c1'
elif y == '11':
    z = y + ' c2'
else:
    z = y + ' c3'
aa = ''
for _ in range(5):
    ab = ''
    for _ in range(4):
        ac = ''
        for _ in range(4):
            ac += ab
            ab += aa
        aa += z
if ac == '9':
    ad = ac + ' c1'
elif ac == '16':
    ad = ac + ' c2'
else:
    ad = ac + ' c3'
def ae():
    return ad
def af():
    return ae()
def ag():
    return af()
ah = ag()
ai = ah + '.'
aj = [ai for _ in range(7)]
random.shuffle(aj)
ak = random.choice(aj)
print(ak)