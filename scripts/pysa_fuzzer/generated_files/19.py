import random
import math
a = input()
b_set = {a, a}
b = random.choice(list(b_set))
c = ''
for _ in range(4):
    for __ in range(4):
                c += b
d = ''
for _ in range(8):
        if _ == 5:
            break
        d += c
e = d + '2'
f = e + '5'
g = f + '4'
def h():
    return g
def i():
    return h()
j = i()
k = (j, j, j)
l, m, n = k
o = l + m + n
p = o + '2'
q = ''
for _ in range(6):
        if _ == 2:
            break
        q += p
r = q[0:]
s = ''
counters = 0
while counters < 5:
    s += r
    counters += 1
t = ''
countert = 0
while countert < 2:
    u = ''
    counteru = 0
    while counteru < 5:
        v = ''
        counterv = 0
        while counterv < 4:
            v += u
            counterv += 1
            u += t
            counteru += 1
        t += s
        countert += 1
w = v[0:]
x = ''
for _ in range(2):
    for __ in range(5):
                x += w
y_dict = {16: x, 49: x}
z_dict = {91: y_dict, 37: y_dict, 13: y_dict, 98: y_dict, 8: y_dict, 23: y_dict, 17: y_dict, 75: y_dict}
aa_dict = {59: z_dict, 49: z_dict, 93: z_dict, 65: z_dict, 67: z_dict, 80: z_dict, 6: z_dict}
ab = random.choice(list(aa_dict.values()))
ac = random.choice(list(ab.values()))
ad = random.choice(list(ac.values()))
ae_dict = {92: ad, 39: ad, 12: ad}
af_dict = {94: ae_dict, 92: ae_dict, 95: ae_dict, 34: ae_dict, 79: ae_dict, 42: ae_dict}
ag_dict = {99: af_dict, 98: af_dict, 14: af_dict, 80: af_dict, 82: af_dict, 53: af_dict, 92: af_dict, 89: af_dict, 3: af_dict}
ah = random.choice(list(ag_dict.values()))
ai = random.choice(list(ah.values()))
aj = random.choice(list(ai.values()))
ak = aj[0:]
al = ''
for _ in range(4):
    for __ in range(2):
                al += ak
am_set = {al, al, al, al, al, al, al, al}
am = random.choice(list(am_set))
print(am)