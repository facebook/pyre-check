import random
import math

a = input()
b = ''
counterb = 0
while counterb < 2:
    c = ''
    counterc = 0
    while counterc < 4:
        d = ''
        counterd = 0
        while counterd < 5:
            d += c
            counterd += 1
            c += b
            counterc += 1
        b += a
        counterb += 1
e_set = {d, d, d, d, d, d, d}
e = random.choice(list(e_set))
f = e[0:]
g = f + '.'
h_list = [g for _ in range(2)]
i = random.choice(h_list)
j_set = {i, i, i}
j = random.choice(list(j_set))
k_dict = {65: j, 73: j, 80: j, 56: j, 26: j, 81: j}
l_dict = {72: k_dict, 33: k_dict, 64: k_dict, 47: k_dict, 38: k_dict, 55: k_dict, 9: k_dict, 63: k_dict}
m_dict = {6: l_dict, 12: l_dict, 46: l_dict}
n = random.choice(list(m_dict.values()))
o = random.choice(list(n.values()))
p = random.choice(list(o.values()))
q = ''
for _ in range(5):
    for __ in range(3):
                q += p
r = q[0:]
s = ''
for _ in range(10):
        if _ == 3:
            break
        s += r
t = f'string {s}'
def u():
    return t
def v():
    return u()
w = v()
x = ''
counterx = 0
while counterx < 4:
    y = ''
    countery = 0
    while countery < 3:
        z = ''
        counterz = 0
        while counterz < 2:
            z += y
            counterz += 1
            y += x
            countery += 1
        x += w
        counterx += 1
aa = ''
for _ in range(5):
    aa += z
ab = ''
counterab = 0
while counterab < 5:
    ac = ''
    counterac = 0
    while counterac < 3:
        ad = ''
        counterad = 0
        while counterad < 2:
            ad += ac
            counterad += 1
            ac += ab
            counterac += 1
        ab += aa
        counterab += 1
ae_list = [ad for _ in range(8)]
af_list = [ae_list for _ in range(4)]
ag_list = [af_list for _ in range(4)]
ah = random.choice(ag_list)
ai = random.choice(ah)
aj = random.choice(ai)
ak = aj + '5'
al = [ak for _ in range(10)]
random.shuffle(al)
am = random.choice(al)
print(am)