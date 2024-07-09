import random
import math

a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g_set = {f, f, f, f, f, f, f, f, f}
g = random.choice(list(g_set))
def h():
    return g
def i():
    return h()
def j():
    return i()
k = j()
l = k + '5'
m = ''
for _ in range(5):
    m += l
n = ''
for _ in range(3):
    o = ''
    for _ in range(2):
        p = ''
        for _ in range(2):
            p += o
            o += n
        n += m
q = p + '.'
r_list = [q for _ in range(4)]
s_list = [r_list for _ in range(5)]
t = random.choice(s_list)
u = random.choice(t)
v = ''
for _ in range(10):
        if _ == 3:
            continue
        v += u
w = ''
for _ in range(2):
    x = ''
    for _ in range(5):
        y = ''
        for _ in range(4):
            y += x
            x += w
        w += v
z = ''
for _ in range(4):
    aa = ''
    for _ in range(3):
        ab = ''
        for _ in range(5):
            ab += aa
            aa += z
        z += y
ac = f'string {ab}'
ad = (ac, ac, ac)
ae, af, ag = ad
ah = ae + af + ag
ai = ah[0:]
def aj():
    return ai
ak = aj()
al = ''
for _ in range(9):
        if _ == 4:
            break
        al += ak
am = f'string {al}'
def an():
    return am
def ao():
    return an()
ap = ao()
print(ap)