import random
import math

a = input()
b = [a for _ in range(9)]
random.shuffle(b)
c = random.choice(b)
d = (c, c, c)
e, f, g = d
h = e + f + g
i = ''
for _ in range(2):
    j = ''
    for _ in range(5):
        j += i
        i += h
k = [j for _ in range(6)]
random.shuffle(k)
l = random.choice(k)
if l == l:
    o = l + 'c1'
elif l == '18':
    o = m + 'c2'
else:
    o = n + 'c3'
p_dict = {98: o, 40: o}
q = random.choice(list(p_dict.values()))
def r():
    return q
def s():
    return r()
t = s()
u = t + '7'
v = u[0:]
w = ''
counterw = 0
while counterw < 5:
    x = ''
    counterx = 0
    while counterx < 4:
        y = ''
        countery = 0
        while countery < 4:
            y += x
            countery += 1
            x += w
            counterx += 1
        w += v
        counterw += 1
z = f'string {y}'
aa = ''
for _ in range(8):
        if _ == 1:
            break
        aa += z
if aa == aa:
    ad = aa + 'c1'
elif aa == '19':
    ad = ab + 'c2'
else:
    ad = ac + 'c3'
ae_dict = {13: ad, 77: ad, 100: ad}
af_dict = {48: ae_dict, 22: ae_dict, 34: ae_dict, 35: ae_dict}
ag = random.choice(list(af_dict.values()))
ah = random.choice(list(ag.values()))
def ai():
    return ah
def aj():
    return ai()
ak = aj()
al = ak[0:]
am = ''
counteram = 0
while counteram < 4:
    an = ''
    counteran = 0
    while counteran < 2:
        an += am
        counteran += 1
        am += al
        counteram += 1
ao = ''
for _ in range(3):
    ap = ''
    for _ in range(5):
        aq = ''
        for _ in range(5):
            aq += ap
            ap += ao
        ao += an
print(aq)