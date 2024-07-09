import random
import math

a = input()
b = a + '6'
c = b + '6'
d = [c for _ in range(8)]
random.shuffle(d)
e = random.choice(d)
f = ''
for _ in range(2):
    g = ''
    for _ in range(3):
        h = ''
        for _ in range(2):
            h += g
            g += f
        f += e
i_set = {h, h, h, h}
i = random.choice(list(i_set))
j = i[0:]
k = ''
for _ in range(4):
    k += j
l = f'string {k}'
m = l + '.'
n_dict = {50: m, 85: m, 1: m, 61: m, 27: m, 2: m, 58: m, 82: m, 82: m, 9: m}
o_dict = {8: n_dict, 52: n_dict, 20: n_dict, 89: n_dict, 70: n_dict}
p_dict = {85: o_dict, 36: o_dict, 93: o_dict, 28: o_dict, 11: o_dict, 45: o_dict, 48: o_dict, 95: o_dict, 87: o_dict, 64: o_dict}
q = random.choice(list(p_dict.values()))
r = random.choice(list(q.values()))
s = random.choice(list(r.values()))
t = [s for _ in range(6)]
random.shuffle(t)
u = random.choice(t)
v = [u for _ in range(6)]
random.shuffle(v)
w = random.choice(v)
x = w[0:]
if x == x:
    aa = x + 'c1'
elif x == '15':
    aa = y + 'c2'
else:
    aa = z + 'c3'
ab = aa + '9'
ac = ab + '4'
ad = ac + '3'
ae = ''
for _ in range(5):
    af = ''
    for _ in range(4):
        ag = ''
        for _ in range(5):
            ag += af
            af += ae
        ae += ad
if ag == ag:
    aj = ag + 'c1'
elif ag == '18':
    aj = ah + 'c2'
else:
    aj = ai + 'c3'
ak = aj + '5'
al = ak + '6'
am = ''
counteram = 0
while counteram < 5:
    an = ''
    counteran = 0
    while counteran < 4:
        ao = ''
        counterao = 0
        while counterao < 4:
            ao += an
            counterao += 1
            an += am
            counteran += 1
        am += al
        counteram += 1
print(ao)