import random
import math
a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g = f + '9'
h = g + '9'
i = f'string {h}'
j_list = [i for _ in range(7)]
k = random.choice(j_list)
l_dict = {38: k, 24: k}
m = random.choice(list(l_dict.values()))
n = m + '8'
o = n[0:]
def p():
    return o
def q():
    return p()
def r():
    return q()
s = r()
t = ''
countert = 0
while countert < 4:
    u = ''
    counteru = 0
    while counteru < 2:
        u += t
        counteru += 1
        t += s
        countert += 1
v = f'string {u}'
w = (v, v, v)
x, y, z = w
aa = x + y + z
ab = ''
for _ in range(3):
    ab += aa
if ab == '6':
    ac = ab + ' c1'
elif ab == '11':
    ac = ab + ' c2'
else:
    ac = ab + ' c3'
ad = f'string {ac}'
ae = ''
for _ in range(4):
    ae += ad
af = (ae, ae, ae)
ag, ah, ai = af
aj = ag + ah + ai
ak = [aj for _ in range(10)]
random.shuffle(ak)
al = random.choice(ak)
am = ''
counteram = 0
while counteram < 5:
    an = ''
    counteran = 0
    while counteran < 3:
        ao = ''
        counterao = 0
        while counterao < 5:
            ao += an
            counterao += 1
            an += am
            counteran += 1
        am += al
        counteram += 1
print(ao)