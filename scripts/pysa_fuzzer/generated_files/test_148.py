import random
import math

a = input()
b = ''
for _ in range(5):
    b += a
c = ''
counterc = 0
while counterc < 5:
    d = ''
    counterd = 0
    while counterd < 3:
        e = ''
        countere = 0
        while countere < 3:
            e += d
            countere += 1
            d += c
            counterd += 1
        c += b
        counterc += 1
f = [e for _ in range(7)]
random.shuffle(f)
g = random.choice(f)
h = ''
for _ in range(5):
    for __ in range(2):
                h += g
i = h + '9'
j = i + '5'
k = j + '8'
l = k + '1'
m = l + '1'
n = m + '6'
o = ''
countero = 0
while countero < 2:
    p = ''
    counterp = 0
    while counterp < 2:
        q = ''
        counterq = 0
        while counterq < 2:
            q += p
            counterq += 1
            p += o
            counterp += 1
        o += n
        countero += 1
if q == q:
    t = q + 'c1'
elif q == '18':
    t = r + 'c2'
else:
    t = s + 'c3'
u = (t, t, t)
v, w, x = u
y = v + w + x
z = ''
counterz = 0
while counterz < 5:
    aa = ''
    counteraa = 0
    while counteraa < 4:
        aa += z
        counteraa += 1
        z += y
        counterz += 1
ab = ''
for _ in range(4):
    for __ in range(4):
                ab += aa
ac = ''
for _ in range(10):
        if _ == 4:
            break
        ac += ab
ad = ''
for _ in range(2):
    ad += ac
ae = (ad, ad, ad)
af, ag, ah = ae
ai = af + ag + ah
aj_list = [ai for _ in range(5)]
ak = random.choice(aj_list)
al = [ak for _ in range(9)]
random.shuffle(al)
am = random.choice(al)
an = am + '.'
print(an)