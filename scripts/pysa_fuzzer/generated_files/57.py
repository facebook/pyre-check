import random
import math
a = input()
b = ''
counterb = 0
while counterb < 5:
    c = ''
    counterc = 0
    while counterc < 5:
        d = ''
        counterd = 0
        while counterd < 5:
            d += c
            counterd += 1
            c += b
            counterc += 1
        b += a
        counterb += 1
def e():
    return d
f = e()
g = [f for _ in range(9)]
random.shuffle(g)
h = random.choice(g)
i = h[0:]
j = i + '.'
def k():
    return j
def l():
    return k()
m = l()
n = ''
for _ in range(4):
    for __ in range(3):
                n += m
o = ''
for _ in range(6):
        if _ == 1:
            continue
        o += n
if o == '8':
    p = o + ' c1'
elif o == '17':
    p = o + ' c2'
else:
    p = o + ' c3'
q = ''
for _ in range(7):
        if _ == 2:
            break
        q += p
r_list = [q for _ in range(2)]
s_list = [r_list for _ in range(10)]
t = random.choice(s_list)
u = random.choice(t)
v_set = {u, u, u, u, u}
v = random.choice(list(v_set))
w = ''
counterw = 0
while counterw < 4:
    x = ''
    counterx = 0
    while counterx < 2:
        y = ''
        countery = 0
        while countery < 5:
            y += x
            countery += 1
            x += w
            counterx += 1
        w += v
        counterw += 1
def z():
    return y
def aa():
    return z()
ab = aa()
ac = ab + '1'
ad = ac + '6'
ae = [ad for _ in range(8)]
random.shuffle(ae)
af = random.choice(ae)
ag_dict = {37: af, 38: af, 19: af, 11: af, 93: af, 69: af, 70: af}
ah = random.choice(list(ag_dict.values()))
ai = ah + '.'
print(ai)