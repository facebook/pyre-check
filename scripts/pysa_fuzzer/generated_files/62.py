import random
import math
a = input()
b = [a for _ in range(10)]
random.shuffle(b)
c = random.choice(b)
d = ''
for _ in range(5):
    e = ''
    for _ in range(4):
        e += d
        d += c
f = f'string {e}'
g = f + '.'
h = [g for _ in range(6)]
random.shuffle(h)
i = random.choice(h)
j_dict = {81: i, 52: i, 64: i, 8: i}
k_dict = {36: j_dict, 18: j_dict, 81: j_dict, 48: j_dict}
l_dict = {95: k_dict, 43: k_dict, 14: k_dict, 52: k_dict, 80: k_dict, 33: k_dict, 58: k_dict, 67: k_dict}
m = random.choice(list(l_dict.values()))
n = random.choice(list(m.values()))
o = random.choice(list(n.values()))
p = ''
for _ in range(3):
    for __ in range(4):
                p += o
q = f'string {p}'
r = [q for _ in range(6)]
random.shuffle(r)
s = random.choice(r)
t = ''
for _ in range(4):
    for __ in range(5):
                t += s
def u():
    return t
v = u()
w = v + '7'
x = w + '2'
def y():
    return x
def z():
    return y()
def aa():
    return z()
ab = aa()
ac = ''
for _ in range(5):
    for __ in range(3):
                ac += ab
ad = f'string {ac}'
ae = ''
for _ in range(5):
    for __ in range(3):
                ae += ad
af_list = [ae for _ in range(7)]
ag_list = [af_list for _ in range(9)]
ah = random.choice(ag_list)
ai = random.choice(ah)
aj = ''
counteraj = 0
while counteraj < 4:
    ak = ''
    counterak = 0
    while counterak < 2:
        al = ''
        counteral = 0
        while counteral < 5:
            al += ak
            counteral += 1
            ak += aj
            counterak += 1
        aj += ai
        counteraj += 1
print(al)