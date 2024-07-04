import random
import math
a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g = ''
for _ in range(6):
        if _ == 4:
            break
        g += f
h = ''
counterh = 0
while counterh < 4:
    i = ''
    counteri = 0
    while counteri < 4:
        j = ''
        counterj = 0
        while counterj < 3:
            j += i
            counterj += 1
            i += h
            counteri += 1
        h += g
        counterh += 1
k = ''
for _ in range(2):
    l = ''
    for _ in range(5):
        l += k
        k += j
m = [l for _ in range(9)]
random.shuffle(m)
n = random.choice(m)
o = n + '8'
p = o + '7'
q = p + '4'
r_set = {q, q, q, q, q, q, q, q}
r = random.choice(list(r_set))
s_dict = {55: r, 37: r, 49: r, 42: r, 88: r, 22: r, 82: r, 93: r, 48: r, 31: r}
t_dict = {26: s_dict, 100: s_dict, 30: s_dict, 28: s_dict, 79: s_dict, 29: s_dict, 71: s_dict, 31: s_dict}
u = random.choice(list(t_dict.values()))
v = random.choice(list(u.values()))
w = ''
for _ in range(3):
    for __ in range(5):
                w += v
def x():
    return w
def y():
    return x()
def z():
    return y()
aa = z()
def ab():
    return aa
ac = ab()
ad = ac + '5'
ae = ad + '2'
af = ae + '8'
ag_list = [af for _ in range(7)]
ah = random.choice(ag_list)
ai = ''
for _ in range(4):
    aj = ''
    for _ in range(4):
        ak = ''
        for _ in range(4):
            ak += aj
            aj += ai
        ai += ah
al = ak + '.'
am = ''
counteram = 0
while counteram < 4:
    an = ''
    counteran = 0
    while counteran < 4:
        ao = ''
        counterao = 0
        while counterao < 5:
            ao += an
            counterao += 1
            an += am
            counteran += 1
        am += al
        counteram += 1
ap = ao + '2'
aq = ap + '2'
if aq == '10':
    ar = aq + ' c1'
elif aq == '11':
    ar = aq + ' c2'
else:
    ar = aq + ' c3'
print(ar)