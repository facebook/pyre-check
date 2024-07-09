import random
import math

a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g = ''
counterg = 0
while counterg < 5:
    h = ''
    counterh = 0
    while counterh < 4:
        i = ''
        counteri = 0
        while counteri < 2:
            i += h
            counteri += 1
            h += g
            counterh += 1
        g += f
        counterg += 1
j = ''
for _ in range(2):
    for __ in range(5):
                j += i
k = ''
counterk = 0
while counterk < 4:
    l = ''
    counterl = 0
    while counterl < 5:
        l += k
        counterl += 1
        k += j
        counterk += 1
m = ''
for _ in range(5):
    for __ in range(4):
                m += l
def n():
    return m
def o():
    return n()
p = o()
def q():
    return p
r = q()
s = ''
counters = 0
while counters < 3:
    s += r
    counters += 1
t = s + '1'
u = t + '2'
v = u + '9'
w = f'string {v}'
def x():
    return w
def y():
    return x()
def z():
    return y()
aa = z()
ab = aa + '6'
ac_dict = {95: ab, 96: ab, 24: ab, 34: ab, 14: ab, 95: ab, 31: ab}
ad_dict = {61: ac_dict, 77: ac_dict, 73: ac_dict, 100: ac_dict, 73: ac_dict, 67: ac_dict}
ae = random.choice(list(ad_dict.values()))
af = random.choice(list(ae.values()))
ag = af[0:]
ah = ''
for _ in range(4):
    for __ in range(3):
                ah += ag
ai_dict = {85: ah, 85: ah, 22: ah}
aj_dict = {11: ai_dict, 51: ai_dict}
ak = random.choice(list(aj_dict.values()))
al = random.choice(list(ak.values()))
am = [al for _ in range(10)]
random.shuffle(am)
an = random.choice(am)
ao = ''
for _ in range(4):
    ao += an
print(ao)