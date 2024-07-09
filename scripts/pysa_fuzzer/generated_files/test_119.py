import random
import math

a = input()
b = ''
counterb = 0
while counterb < 4:
    c = ''
    counterc = 0
    while counterc < 3:
        d = ''
        counterd = 0
        while counterd < 3:
            d += c
            counterd += 1
            c += b
            counterc += 1
        b += a
        counterb += 1
e = ''
for _ in range(5):
    for __ in range(4):
                e += d
def f():
    return e
def g():
    return f()
def h():
    return g()
i = h()
j = ''
for _ in range(4):
    for __ in range(5):
                j += i
k = (j, j, j)
l, m, n = k
o = l + m + n
p = ''
for _ in range(3):
    for __ in range(4):
                p += o
q = p + '5'
r = q + '4'
s = r + '8'
t_list = [s for _ in range(10)]
u_list = [t_list for _ in range(4)]
v_list = [u_list for _ in range(10)]
w = random.choice(v_list)
x = random.choice(w)
y = random.choice(x)
z = ''
for _ in range(5):
    for __ in range(2):
                z += y
aa = [z for _ in range(10)]
random.shuffle(aa)
ab = random.choice(aa)
ac = ''
for _ in range(8):
        if _ == 2:
            break
        ac += ab
ad = ''
counterad = 0
while counterad < 4:
    ad += ac
    counterad += 1
ae = ''
for _ in range(8):
        if _ == 2:
            continue
        ae += ad
af = ae + '9'
ag = af + '8'
ah = ag + '8'
ai = ah + '.'
aj = ''
counteraj = 0
while counteraj < 2:
    ak = ''
    counterak = 0
    while counterak < 3:
        al = ''
        counteral = 0
        while counteral < 3:
            al += ak
            counteral += 1
            ak += aj
            counterak += 1
        aj += ai
        counteraj += 1
am = f'string {al}'
an = ''
counteran = 0
while counteran < 2:
    ao = ''
    counterao = 0
    while counterao < 2:
        ap = ''
        counterap = 0
        while counterap < 2:
            ap += ao
            counterap += 1
            ao += an
            counterao += 1
        an += am
        counteran += 1
print(ap)