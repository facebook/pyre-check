import random
import math

a = input()
b = ''
for _ in range(2):
    for __ in range(5):
                b += a
c = [b for _ in range(6)]
random.shuffle(c)
d = random.choice(c)
def e():
    return d
f = e()
g = ''
for _ in range(10):
        if _ == 1:
            continue
        g += f
h_list = [g for _ in range(5)]
i_list = [h_list for _ in range(5)]
j = random.choice(i_list)
k = random.choice(j)
l = k[0:]
m = ''
for _ in range(5):
        if _ == 2:
            break
        m += l
n = [m for _ in range(6)]
random.shuffle(n)
o = random.choice(n)
p = ''
for _ in range(5):
    for __ in range(2):
                p += o
def q():
    return p
def r():
    return q()
s = r()
t = s[0:]
u = ''
for _ in range(8):
        if _ == 2:
            break
        u += t
v = u + '5'
w = [v for _ in range(10)]
random.shuffle(w)
x = random.choice(w)
y = ''
for _ in range(3):
    z = ''
    for _ in range(2):
        z += y
        y += x
aa_dict = {100: z, 88: z, 45: z, 60: z, 67: z, 59: z}
ab_dict = {11: aa_dict, 25: aa_dict}
ac_dict = {86: ab_dict, 96: ab_dict, 34: ab_dict, 82: ab_dict, 2: ab_dict, 58: ab_dict}
ad = random.choice(list(ac_dict.values()))
ae = random.choice(list(ad.values()))
af = random.choice(list(ae.values()))
ag = af + '4'
ah = ag + '4'
ai_dict = {88: ah, 21: ah, 53: ah, 11: ah, 58: ah, 61: ah, 79: ah, 1: ah, 74: ah}
aj = random.choice(list(ai_dict.values()))
print(aj)