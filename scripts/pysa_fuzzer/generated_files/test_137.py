import random
import math

a = input()
b = ''
counterb = 0
while counterb < 4:
    c = ''
    counterc = 0
    while counterc < 5:
        d = ''
        counterd = 0
        while counterd < 5:
            d += c
            counterd += 1
            c += b
            counterc += 1
        b += a
        counterb += 1
def e():
    return d
def f():
    return e()
def g():
    return f()
h = g()
i = (h, h, h)
j, k, l = i
m = j + k + l
n_dict = {2: m, 24: m, 11: m, 66: m, 5: m, 17: m, 90: m, 83: m, 86: m}
o_dict = {84: n_dict, 97: n_dict, 100: n_dict, 42: n_dict}
p_dict = {40: o_dict, 10: o_dict, 76: o_dict, 28: o_dict, 26: o_dict}
q = random.choice(list(p_dict.values()))
r = random.choice(list(q.values()))
s = random.choice(list(r.values()))
t = ''
for _ in range(6):
        if _ == 5:
            continue
        t += s
u = [t for _ in range(8)]
random.shuffle(u)
v = random.choice(u)
w = ''
for _ in range(3):
    for __ in range(2):
                w += v
x = [w for _ in range(8)]
random.shuffle(x)
y = random.choice(x)
z = y[0:]
aa_set = {z, z}
aa = random.choice(list(aa_set))
ab = aa + '.'
ac = [ab for _ in range(9)]
random.shuffle(ac)
ad = random.choice(ac)
ae = ''
for _ in range(4):
    af = ''
    for _ in range(4):
        ag = ''
        for _ in range(2):
            ag += af
            af += ae
        ae += ad
ah_dict = {19: ag, 89: ag, 35: ag, 32: ag}
ai_dict = {21: ah_dict, 96: ah_dict, 72: ah_dict, 1: ah_dict, 65: ah_dict, 5: ah_dict, 14: ah_dict, 17: ah_dict, 74: ah_dict}
aj = random.choice(list(ai_dict.values()))
ak = random.choice(list(aj.values()))
al = ''
for _ in range(8):
        if _ == 2:
            break
        al += ak
am_list = [al for _ in range(4)]
an_list = [am_list for _ in range(7)]
ao = random.choice(an_list)
ap = random.choice(ao)
aq = ''
for _ in range(8):
        if _ == 4:
            break
        aq += ap
ar = ''
counterar = 0
while counterar < 5:
    at = ''
    counterat = 0
    while counterat < 3:
        au = ''
        counterau = 0
        while counterau < 4:
            au += at
            counterau += 1
            at += ar
            counterat += 1
        ar += aq
        counterar += 1
print(au)