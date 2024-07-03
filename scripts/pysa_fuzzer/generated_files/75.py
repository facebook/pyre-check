import random
import math
a = input()
b_list = [a for _ in range(2)]
c_list = [b_list for _ in range(6)]
d_list = [c_list for _ in range(10)]
e = random.choice(d_list)
f = random.choice(e)
g = random.choice(f)
if g == '1':
    h = g + ' c1'
elif g == '15':
    h = g + ' c2'
else:
    h = g + ' c3'
def i():
    return h
j = i()
k = j + '4'
l = k + '8'
m = l[0:]
def n():
    return m
def o():
    return n()
def p():
    return o()
q = p()
r = ''
for _ in range(3):
    for __ in range(4):
                r += q
def s():
    return r
def t():
    return s()
u = t()
v_set = {u, u, u, u}
v = random.choice(list(v_set))
w = [v for _ in range(9)]
random.shuffle(w)
x = random.choice(w)
y = ''
for _ in range(5):
        if _ == 4:
            break
        y += x
z_set = {y, y, y, y, y, y, y, y, y}
z = random.choice(list(z_set))
aa = ''
for _ in range(3):
    ab = ''
    for _ in range(4):
        ab += aa
        aa += z
if ab == '2':
    ac = ab + ' c1'
elif ab == '11':
    ac = ab + ' c2'
else:
    ac = ab + ' c3'
ad = ac[0:]
ae = ''
for _ in range(4):
    ae += ad
af = ''
for _ in range(3):
    ag = ''
    for _ in range(4):
        ag += af
        af += ae
ah_list = [ag for _ in range(2)]
ai_list = [ah_list for _ in range(2)]
aj = random.choice(ai_list)
ak = random.choice(aj)
print(ak)