import random
import math

a = input()
if a == a:
    d = a + 'c1'
elif a == '19':
    d = b + 'c2'
else:
    d = c + 'c3'
e = ''
countere = 0
while countere < 4:
    f = ''
    counterf = 0
    while counterf < 4:
        g = ''
        counterg = 0
        while counterg < 4:
            g += f
            counterg += 1
            f += e
            counterf += 1
        e += d
        countere += 1
h = ''
counterh = 0
while counterh < 3:
    i = ''
    counteri = 0
    while counteri < 4:
        j = ''
        counterj = 0
        while counterj < 5:
            j += i
            counterj += 1
            i += h
            counteri += 1
        h += g
        counterh += 1
k = j + '.'
l = k[0:]
m = ''
for _ in range(3):
    n = ''
    for _ in range(5):
        o = ''
        for _ in range(3):
            o += n
            n += m
        m += l
p = ''
for _ in range(7):
        if _ == 3:
            continue
        p += o
q_dict = {28: p, 54: p, 42: p, 84: p, 47: p, 31: p, 3: p, 50: p, 97: p}
r_dict = {54: q_dict, 35: q_dict, 73: q_dict}
s = random.choice(list(r_dict.values()))
t = random.choice(list(s.values()))
u = [t for _ in range(8)]
random.shuffle(u)
v = random.choice(u)
w_set = {v, v, v, v, v, v, v, v, v, v}
w = random.choice(list(w_set))
def x():
    return w
y = x()
if y == y:
    ab = y + 'c1'
elif y == '13':
    ab = z + 'c2'
else:
    ab = aa + 'c3'
if ab == ab:
    ae = ab + 'c1'
elif ab == '11':
    ae = ac + 'c2'
else:
    ae = ad + 'c3'
af = ae + '7'
ag = af + '1'
ah = ag + '3'
ai_dict = {3: ah, 2: ah, 22: ah, 48: ah, 100: ah, 59: ah, 59: ah, 28: ah}
aj = random.choice(list(ai_dict.values()))
ak = ''
counterak = 0
while counterak < 5:
    al = ''
    counteral = 0
    while counteral < 3:
        al += ak
        counteral += 1
        ak += aj
        counterak += 1
am = ''
counteram = 0
while counteram < 3:
    an = ''
    counteran = 0
    while counteran < 2:
        ao = ''
        counterao = 0
        while counterao < 3:
            ao += an
            counterao += 1
            an += am
            counteran += 1
        am += al
        counteram += 1
ap_set = {ao, ao, ao, ao, ao, ao, ao}
ap = random.choice(list(ap_set))
print(ap)