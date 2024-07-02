import random
import math
a = input()
b = ''
counterb = 0
while counterb < 5:
    b += a
    counterb += 1
c = b + '3'
d = c + '2'
e = d + '7'
f = ''
for _ in range(10):
        if _ == 5:
            break
        f += e
g_list = [f for _ in range(4)]
h_list = [g_list for _ in range(2)]
i_list = [h_list for _ in range(6)]
j = random.choice(i_list)
k = random.choice(j)
l = random.choice(k)
m = ''
for _ in range(4):
    for __ in range(5):
                m += l
n = ''
for _ in range(9):
        if _ == 5:
            break
        n += m
def o():
    return n
def p():
    return o()
def q():
    return p()
r = q()
s = f'string {r}'
def t():
    return s
def u():
    return t()
v = u()
w = v + '1'
x = [w for _ in range(8)]
random.shuffle(x)
y = random.choice(x)
z = ''
for _ in range(5):
        if _ == 5:
            break
        z += y
aa = ''
counteraa = 0
while counteraa < 4:
    ab = ''
    counterab = 0
    while counterab < 3:
        ab += aa
        counterab += 1
        aa += z
        counteraa += 1
ac = ab + '.'
ad = ''
for _ in range(3):
    ad += ac
ae = ad[0:]
def af():
    return ae
def ag():
    return af()
ah = ag()
ai = ''
for _ in range(3):
    aj = ''
    for _ in range(4):
        ak = ''
        for _ in range(4):
            ak += aj
            aj += ai
        ai += ah
print(ak)