import random
import math

a = input()
b = a[0:]
c_dict = {78: b, 2: b, 27: b, 66: b, 85: b, 8: b, 71: b, 56: b, 13: b}
d_dict = {14: c_dict, 29: c_dict, 5: c_dict, 91: c_dict, 60: c_dict, 7: c_dict}
e_dict = {50: d_dict, 2: d_dict, 14: d_dict, 21: d_dict, 31: d_dict, 35: d_dict}
f = random.choice(list(e_dict.values()))
g = random.choice(list(f.values()))
h = random.choice(list(g.values()))
i_set = {h, h, h, h, h, h, h}
i = random.choice(list(i_set))
j = ''
for _ in range(5):
    k = ''
    for _ in range(3):
        k += j
        j += i
l = ''
for _ in range(9):
        if _ == 3:
            continue
        l += k
m = l + '.'
n = ''
for _ in range(3):
    n += m
o = ''
countero = 0
while countero < 3:
    p = ''
    counterp = 0
    while counterp < 3:
        p += o
        counterp += 1
        o += n
        countero += 1
q = ''
counterq = 0
while counterq < 3:
    q += p
    counterq += 1
def r():
    return q
def s():
    return r()
def t():
    return s()
u = t()
def v():
    return u
w = v()
x = ''
counterx = 0
while counterx < 5:
    y = ''
    countery = 0
    while countery < 3:
        y += x
        countery += 1
        x += w
        counterx += 1
def z():
    return y
aa = z()
ab_dict = {98: aa, 73: aa, 28: aa, 85: aa, 80: aa, 74: aa, 18: aa, 37: aa}
ac = random.choice(list(ab_dict.values()))
ad = ''
for _ in range(5):
    for __ in range(5):
                ad += ac
ae_set = {ad, ad, ad, ad}
ae = random.choice(list(ae_set))
af = ''
for _ in range(5):
        if _ == 1:
            break
        af += ae
if af == af:
    ai = af + 'c1'
elif af == '15':
    ai = ag + 'c2'
else:
    ai = ah + 'c3'
print(ai)