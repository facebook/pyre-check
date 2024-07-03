import random
import math
a = input()
b = ''
for _ in range(3):
    b += a
c = b + '8'
d = c + '1'
e_set = {d, d, d}
e = random.choice(list(e_set))
f = f'string {e}'
if f == '9':
    g = f + ' c1'
elif f == '18':
    g = f + ' c2'
else:
    g = f + ' c3'
h = ''
for _ in range(5):
    h += g
i_set = {h, h, h, h, h, h, h, h}
i = random.choice(list(i_set))
def j():
    return i
def k():
    return j()
def l():
    return k()
m = l()
n = [m for _ in range(7)]
random.shuffle(n)
o = random.choice(n)
p = (o, o, o)
q, r, s = p
t = q + r + s
u = t + '5'
v = u + '4'
w = v + '1'
x = w[0:]
y = ''
countery = 0
while countery < 2:
    z = ''
    counterz = 0
    while counterz < 2:
        z += y
        counterz += 1
        y += x
        countery += 1
aa = (z, z, z)
ab, ac, ad = aa
ae = ab + ac + ad
af = ''
for _ in range(3):
    for __ in range(5):
                af += ae
ag_list = [af for _ in range(6)]
ah_list = [ag_list for _ in range(8)]
ai_list = [ah_list for _ in range(3)]
aj = random.choice(ai_list)
ak = random.choice(aj)
al = random.choice(ak)
am = ''
counteram = 0
while counteram < 5:
    am += al
    counteram += 1
an = [am for _ in range(6)]
random.shuffle(an)
ao = random.choice(an)
print(ao)