import random
import math

a = input()
b_list = [a for _ in range(5)]
c = random.choice(b_list)
d = c + '.'
e = d + '5'
f = e + '9'
g = ''
for _ in range(4):
    g += f
h = ''
counterh = 0
while counterh < 3:
    i = ''
    counteri = 0
    while counteri < 4:
        j = ''
        counterj = 0
        while counterj < 4:
            j += i
            counterj += 1
            i += h
            counteri += 1
        h += g
        counterh += 1
if j == j:
    m = j + 'c1'
elif j == '20':
    m = k + 'c2'
else:
    m = l + 'c3'
n = f'string {m}'
o = ''
for _ in range(2):
    for __ in range(5):
                o += n
if o == o:
    r = o + 'c1'
elif o == '12':
    r = p + 'c2'
else:
    r = q + 'c3'
if r == r:
    u = r + 'c1'
elif r == '18':
    u = s + 'c2'
else:
    u = t + 'c3'
def v():
    return u
def w():
    return v()
def x():
    return w()
y = x()
z = [y for _ in range(5)]
random.shuffle(z)
aa = random.choice(z)
ab = ''
for _ in range(6):
        if _ == 1:
            continue
        ab += aa
ac = ''
for _ in range(5):
    ad = ''
    for _ in range(5):
        ad += ac
        ac += ab
ae = ''
counterae = 0
while counterae < 3:
    af = ''
    counteraf = 0
    while counteraf < 3:
        ag = ''
        counterag = 0
        while counterag < 5:
            ag += af
            counterag += 1
            af += ae
            counteraf += 1
        ae += ad
        counterae += 1
ah_dict = {91: ag, 41: ag, 14: ag, 67: ag, 47: ag, 32: ag, 48: ag, 84: ag, 94: ag, 45: ag}
ai = random.choice(list(ah_dict.values()))
aj = ai[0:]
ak = ''
for _ in range(5):
    for __ in range(5):
                ak += aj
print(ak)