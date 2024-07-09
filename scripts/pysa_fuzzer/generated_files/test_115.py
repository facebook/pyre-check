import random
import math

a = input()
b = ''
counterb = 0
while counterb < 5:
    c = ''
    counterc = 0
    while counterc < 4:
        d = ''
        counterd = 0
        while counterd < 3:
            d += c
            counterd += 1
            c += b
            counterc += 1
        b += a
        counterb += 1
e = d + '.'
f = [e for _ in range(9)]
random.shuffle(f)
g = random.choice(f)
h = ''
for _ in range(4):
    i = ''
    for _ in range(4):
        i += h
        h += g
j = i + '.'
k_list = [j for _ in range(2)]
l_list = [k_list for _ in range(9)]
m_list = [l_list for _ in range(4)]
n = random.choice(m_list)
o = random.choice(n)
p = random.choice(o)
if p == p:
    s = p + 'c1'
elif p == '18':
    s = q + 'c2'
else:
    s = r + 'c3'
t = ''
for _ in range(7):
        if _ == 1:
            continue
        t += s
if t == t:
    w = t + 'c1'
elif t == '12':
    w = u + 'c2'
else:
    w = v + 'c3'
def x():
    return w
def y():
    return x()
def z():
    return y()
aa = z()
ab_dict = {24: aa, 61: aa, 74: aa, 30: aa, 95: aa, 64: aa, 52: aa, 37: aa, 83: aa}
ac_dict = {11: ab_dict, 62: ab_dict, 98: ab_dict}
ad = random.choice(list(ac_dict.values()))
ae = random.choice(list(ad.values()))
af_list = [ae for _ in range(8)]
ag_list = [af_list for _ in range(7)]
ah = random.choice(ag_list)
ai = random.choice(ah)
def aj():
    return ai
ak = aj()
al_set = {ak, ak, ak}
al = random.choice(list(al_set))
am = al + '.'
def an():
    return am
def ao():
    return an()
ap = ao()
aq = ap + '8'
ar = aq + '5'
at = ''
counterat = 0
while counterat < 5:
    au = ''
    counterau = 0
    while counterau < 4:
        av = ''
        counterav = 0
        while counterav < 2:
            av += au
            counterav += 1
            au += at
            counterau += 1
        at += ar
        counterat += 1
print(av)