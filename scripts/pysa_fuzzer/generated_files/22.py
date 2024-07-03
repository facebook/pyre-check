import random
import math
a = input()
b = a + '.'
c = b[0:]
d = ''
for _ in range(3):
    e = ''
    for _ in range(2):
        e += d
        d += c
f_dict = {38: e, 34: e, 58: e}
g_dict = {95: f_dict, 23: f_dict, 20: f_dict, 17: f_dict}
h_dict = {14: g_dict, 13: g_dict, 46: g_dict, 78: g_dict, 54: g_dict, 59: g_dict, 49: g_dict, 27: g_dict, 19: g_dict}
i = random.choice(list(h_dict.values()))
j = random.choice(list(i.values()))
k = random.choice(list(j.values()))
l = [k for _ in range(6)]
random.shuffle(l)
m = random.choice(l)
n_dict = {25: m, 15: m, 93: m, 66: m, 68: m, 32: m, 83: m, 33: m}
o_dict = {15: n_dict, 65: n_dict}
p_dict = {93: o_dict, 64: o_dict, 67: o_dict, 38: o_dict, 76: o_dict, 9: o_dict, 48: o_dict, 40: o_dict}
q = random.choice(list(p_dict.values()))
r = random.choice(list(q.values()))
s = random.choice(list(r.values()))
t = ''
for _ in range(3):
    u = ''
    for _ in range(5):
        u += t
        t += s
v = ''
counterv = 0
while counterv < 3:
    w = ''
    counterw = 0
    while counterw < 4:
        w += v
        counterw += 1
        v += u
        counterv += 1
x = w + '.'
if x == '4':
    y = x + ' c1'
elif x == '11':
    y = x + ' c2'
else:
    y = x + ' c3'
def z():
    return y
def aa():
    return z()
def ab():
    return aa()
ac = ab()
ad = [ac for _ in range(7)]
random.shuffle(ad)
ae = random.choice(ad)
af = ae + '8'
ag = af + '2'
ah = ag + '5'
ai = ah + '2'
aj = ''
counteraj = 0
while counteraj < 2:
    ak = ''
    counterak = 0
    while counterak < 4:
        al = ''
        counteral = 0
        while counteral < 4:
            al += ak
            counteral += 1
            ak += aj
            counterak += 1
        aj += ai
        counteraj += 1
am = f'string {al}'
an = ''
for _ in range(4):
    for __ in range(2):
                an += am
ao_dict = {43: an, 96: an, 79: an, 68: an, 67: an, 8: an}
ap_dict = {42: ao_dict, 27: ao_dict, 64: ao_dict, 14: ao_dict, 56: ao_dict, 38: ao_dict, 60: ao_dict, 35: ao_dict, 21: ao_dict}
aq = random.choice(list(ap_dict.values()))
ar = random.choice(list(aq.values()))
print(ar)