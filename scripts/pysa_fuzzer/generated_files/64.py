import random
import math
a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
if f == '3':
    g = f + ' c1'
elif f == '16':
    g = f + ' c2'
else:
    g = f + ' c3'
h = g + '.'
i = h + '.'
j = i + '.'
k = ''
for _ in range(2):
    l = ''
    for _ in range(4):
        m = ''
        for _ in range(3):
            m += l
            l += k
        k += j
n = m + '.'
o = n + '6'
p = o + '8'
q = ''
for _ in range(3):
    for __ in range(5):
                q += p
r = q[0:]
s = ''
for _ in range(2):
    for __ in range(4):
                s += r
t = ''
for _ in range(6):
        if _ == 5:
            break
        t += s
u_dict = {82: t, 1: t, 64: t, 32: t, 51: t, 70: t, 29: t, 47: t}
v_dict = {94: u_dict, 58: u_dict, 72: u_dict, 100: u_dict, 26: u_dict, 59: u_dict, 33: u_dict, 46: u_dict, 89: u_dict}
w_dict = {51: v_dict, 98: v_dict, 93: v_dict, 11: v_dict, 28: v_dict, 89: v_dict, 84: v_dict, 83: v_dict, 63: v_dict}
x = random.choice(list(w_dict.values()))
y = random.choice(list(x.values()))
z = random.choice(list(y.values()))
aa = z[0:]
ab_dict = {44: aa, 70: aa, 91: aa, 10: aa, 53: aa}
ac = random.choice(list(ab_dict.values()))
ad_dict = {44: ac, 70: ac, 47: ac, 86: ac, 73: ac, 89: ac, 60: ac, 10: ac}
ae_dict = {99: ad_dict, 46: ad_dict, 18: ad_dict, 38: ad_dict, 86: ad_dict}
af_dict = {12: ae_dict, 95: ae_dict, 8: ae_dict, 78: ae_dict, 16: ae_dict, 77: ae_dict, 3: ae_dict, 16: ae_dict, 96: ae_dict, 48: ae_dict}
ag = random.choice(list(af_dict.values()))
ah = random.choice(list(ag.values()))
ai = random.choice(list(ah.values()))
aj = ''
counteraj = 0
while counteraj < 3:
    ak = ''
    counterak = 0
    while counterak < 5:
        al = ''
        counteral = 0
        while counteral < 5:
            al += ak
            counteral += 1
            ak += aj
            counterak += 1
        aj += ai
        counteraj += 1
am = ''
counteram = 0
while counteram < 2:
    an = ''
    counteran = 0
    while counteran < 5:
        an += am
        counteran += 1
        am += al
        counteram += 1
print(an)