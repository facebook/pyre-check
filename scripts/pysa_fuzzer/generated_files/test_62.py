import random
import math

a = input()
b = ''
counterb = 0
while counterb < 2:
    c = ''
    counterc = 0
    while counterc < 3:
        d = ''
        counterd = 0
        while counterd < 4:
            d += c
            counterd += 1
            c += b
            counterc += 1
        b += a
        counterb += 1
e = [d for _ in range(10)]
random.shuffle(e)
f = random.choice(e)
g = ''
for _ in range(9):
        if _ == 4:
            continue
        g += f
h_dict = {54: g, 81: g, 79: g}
i_dict = {76: h_dict, 35: h_dict, 39: h_dict, 13: h_dict}
j = random.choice(list(i_dict.values()))
k = random.choice(list(j.values()))
l = k + '.'
def m():
    return l
def n():
    return m()
o = n()
if o == o:
    r = o + 'c1'
elif o == '18':
    r = p + 'c2'
else:
    r = q + 'c3'
s_set = {r, r, r, r, r, r, r}
s = random.choice(list(s_set))
def t():
    return s
def u():
    return t()
def v():
    return u()
w = v()
if w == w:
    z = w + 'c1'
elif w == '17':
    z = x + 'c2'
else:
    z = y + 'c3'
aa_dict = {1: z, 66: z, 20: z, 1: z, 35: z, 11: z, 40: z, 65: z}
ab_dict = {52: aa_dict, 61: aa_dict, 59: aa_dict, 8: aa_dict, 82: aa_dict, 63: aa_dict, 20: aa_dict, 66: aa_dict, 71: aa_dict}
ac_dict = {10: ab_dict, 79: ab_dict, 24: ab_dict, 27: ab_dict, 98: ab_dict, 98: ab_dict, 7: ab_dict, 77: ab_dict, 65: ab_dict, 56: ab_dict}
ad = random.choice(list(ac_dict.values()))
ae = random.choice(list(ad.values()))
af = random.choice(list(ae.values()))
ag = ''
counterag = 0
while counterag < 5:
    ah = ''
    counterah = 0
    while counterah < 5:
        ah += ag
        counterah += 1
        ag += af
        counterag += 1
ai = ''
for _ in range(9):
        if _ == 3:
            break
        ai += ah
aj = ''
for _ in range(4):
    ak = ''
    for _ in range(3):
        ak += aj
        aj += ai
al = ak + '9'
am_set = {al, al, al, al, al, al, al, al}
am = random.choice(list(am_set))
an = am + '7'
ao = an + '4'
if ao == ao:
    ar = ao + 'c1'
elif ao == '11':
    ar = ap + 'c2'
else:
    ar = aq + 'c3'
print(ar)