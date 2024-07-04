import random
import math
a = input()
b_dict = {72: a, 2: a, 88: a}
c_dict = {19: b_dict, 57: b_dict, 71: b_dict}
d = random.choice(list(c_dict.values()))
e = random.choice(list(d.values()))
if e == '4':
    f = e + ' c1'
elif e == '11':
    f = e + ' c2'
else:
    f = e + ' c3'
def g():
    return f
def h():
    return g()
def i():
    return h()
j = i()
k = ''
for _ in range(8):
        if _ == 3:
            continue
        k += j
l = ''
counterl = 0
while counterl < 4:
    m = ''
    counterm = 0
    while counterm < 2:
        n = ''
        countern = 0
        while countern < 2:
            n += m
            countern += 1
            m += l
            counterm += 1
        l += k
        counterl += 1
o = ''
for _ in range(2):
    for __ in range(3):
                o += n
p = ''
counterp = 0
while counterp < 3:
    q = ''
    counterq = 0
    while counterq < 4:
        r = ''
        counterr = 0
        while counterr < 3:
            r += q
            counterr += 1
            q += p
            counterq += 1
        p += o
        counterp += 1
s = r + '6'
t = s + '6'
u_list = [t for _ in range(10)]
v_list = [u_list for _ in range(7)]
w_list = [v_list for _ in range(4)]
x = random.choice(w_list)
y = random.choice(x)
z = random.choice(y)
aa = ''
for _ in range(2):
    for __ in range(3):
                aa += z
ab = aa + '.'
ac_list = [ab for _ in range(3)]
ad = random.choice(ac_list)
ae = ''
for _ in range(3):
    for __ in range(5):
                ae += ad
af = f'string {ae}'
ag_list = [af for _ in range(7)]
ah_list = [ag_list for _ in range(5)]
ai = random.choice(ah_list)
aj = random.choice(ai)
def ak():
    return aj
def al():
    return ak()
am = al()
an = ''
for _ in range(8):
        if _ == 4:
            continue
        an += am
ao = ''
for _ in range(4):
    ap = ''
    for _ in range(3):
        aq = ''
        for _ in range(4):
            aq += ap
            ap += ao
        ao += an
print(aq)