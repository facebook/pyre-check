import random
import math

a = input()
b = a + '7'
c = b + '1'
d = c + '4'
e = [d for _ in range(9)]
random.shuffle(e)
f = random.choice(e)
g = f + '1'
h = g + '4'
i_list = [h for _ in range(4)]
j = random.choice(i_list)
k_list = [j for _ in range(5)]
l = random.choice(k_list)
def m():
    return l
def n():
    return m()
def o():
    return n()
p = o()
q = p[0:]
r = ''
counterr = 0
while counterr < 3:
    s = ''
    counters = 0
    while counters < 4:
        s += r
        counters += 1
        r += q
        counterr += 1
def t():
    return s
def u():
    return t()
def v():
    return u()
w = v()
x_set = {w, w, w, w, w, w, w, w, w}
x = random.choice(list(x_set))
y = ''
for _ in range(2):
    for __ in range(3):
                y += x
def z():
    return y
aa = z()
ab = aa + '8'
ac = ab + '2'
ad = ''
counterad = 0
while counterad < 2:
    ad += ac
    counterad += 1
ae = ''
for _ in range(2):
    for __ in range(4):
                ae += ad
def af():
    return ae
def ag():
    return af()
ah = ag()
ai = ''
for _ in range(8):
        if _ == 2:
            continue
        ai += ah
aj_dict = {68: ai, 55: ai, 34: ai, 20: ai, 59: ai, 1: ai, 65: ai}
ak_dict = {64: aj_dict, 57: aj_dict, 35: aj_dict}
al = random.choice(list(ak_dict.values()))
am = random.choice(list(al.values()))
print(am)