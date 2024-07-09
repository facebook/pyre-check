import random
import math

a = input()
b = ''
for _ in range(4):
    c = ''
    for _ in range(3):
        c += b
        b += a
d = c + '7'
e = d + '3'
f = e + '2'
g = ''
for _ in range(5):
        if _ == 5:
            break
        g += f
h = f'string {g}'
i = ''
for _ in range(4):
    j = ''
    for _ in range(5):
        j += i
        i += h
k = ''
counterk = 0
while counterk < 2:
    k += j
    counterk += 1
l = [k for _ in range(10)]
random.shuffle(l)
m = random.choice(l)
n_set = {m, m}
n = random.choice(list(n_set))
o_list = [n for _ in range(4)]
p_list = [o_list for _ in range(3)]
q = random.choice(p_list)
r = random.choice(q)
s = r + '6'
t = s + '3'
u = f'string {t}'
v = ''
for _ in range(4):
    for __ in range(2):
                v += u
def w():
    return v
def x():
    return w()
y = x()
def z():
    return y
def aa():
    return z()
def ab():
    return aa()
ac = ab()
ad = (ac, ac, ac)
ae, af, ag = ad
ah = ae + af + ag
ai = ''
counterai = 0
while counterai < 3:
    ai += ah
    counterai += 1
if ai == ai:
    al = ai + 'c1'
elif ai == '19':
    al = aj + 'c2'
else:
    al = ak + 'c3'
def am():
    return al
def an():
    return am()
ao = an()
print(ao)