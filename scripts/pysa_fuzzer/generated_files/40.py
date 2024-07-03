import random
import math
a = input()
b = ''
for _ in range(5):
        if _ == 4:
            continue
        b += a
c = (b, b, b)
d, e, f = c
g = d + e + f
h_dict = {90: g, 77: g, 35: g, 20: g, 29: g, 56: g}
i = random.choice(list(h_dict.values()))
j = ''
for _ in range(7):
        if _ == 2:
            break
        j += i
k = ''
for _ in range(5):
        if _ == 5:
            continue
        k += j
l = ''
for _ in range(8):
        if _ == 2:
            break
        l += k
def m():
    return l
def n():
    return m()
def o():
    return n()
p = o()
def q():
    return p
def r():
    return q()
def s():
    return r()
t = s()
u = f'string {t}'
def v():
    return u
w = v()
x_dict = {22: w, 84: w}
y_dict = {25: x_dict, 75: x_dict, 41: x_dict, 43: x_dict, 20: x_dict, 76: x_dict, 97: x_dict}
z_dict = {80: y_dict, 39: y_dict, 71: y_dict}
aa = random.choice(list(z_dict.values()))
ab = random.choice(list(aa.values()))
ac = random.choice(list(ab.values()))
ad = [ac for _ in range(5)]
random.shuffle(ad)
ae = random.choice(ad)
af = f'string {ae}'
ag = [af for _ in range(6)]
random.shuffle(ag)
ah = random.choice(ag)
ai = ''
for _ in range(5):
    for __ in range(3):
                ai += ah
aj = ''
for _ in range(7):
        if _ == 2:
            continue
        aj += ai
ak = aj + '2'
al = ak + '6'
am = (al, al, al)
an, ao, ap = am
aq = an + ao + ap
print(aq)