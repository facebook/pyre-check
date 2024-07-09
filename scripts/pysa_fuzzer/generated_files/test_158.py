import random
import math

a = input()
b = [a for _ in range(8)]
random.shuffle(b)
c = random.choice(b)
d_list = [c for _ in range(9)]
e_list = [d_list for _ in range(3)]
f_list = [e_list for _ in range(3)]
g = random.choice(f_list)
h = random.choice(g)
i = random.choice(h)
def j():
    return i
def k():
    return j()
l = k()
m = ''
counterm = 0
while counterm < 3:
    n = ''
    countern = 0
    while countern < 4:
        n += m
        countern += 1
        m += l
        counterm += 1
o = n + '.'
p = ''
for _ in range(3):
    q = ''
    for _ in range(5):
        r = ''
        for _ in range(3):
            r += q
            q += p
        p += o
s = ''
for _ in range(4):
    s += r
t = ''
countert = 0
while countert < 3:
    t += s
    countert += 1
u = t[0:]
v = u[0:]
w = (v, v, v)
x, y, z = w
aa = x + y + z
if aa == aa:
    ad = aa + 'c1'
elif aa == '17':
    ad = ab + 'c2'
else:
    ad = ac + 'c3'
ae = ad + '5'
af_list = [ae for _ in range(7)]
ag = random.choice(af_list)
ah = ''
counterah = 0
while counterah < 3:
    ai = ''
    counterai = 0
    while counterai < 2:
        aj = ''
        counteraj = 0
        while counteraj < 2:
            aj += ai
            counteraj += 1
            ai += ah
            counterai += 1
        ah += ag
        counterah += 1
ak = f'string {aj}'
al_dict = {20: ak, 48: ak}
am = random.choice(list(al_dict.values()))
an = ''
counteran = 0
while counteran < 3:
    ao = ''
    counterao = 0
    while counterao < 4:
        ao += an
        counterao += 1
        an += am
        counteran += 1
print(ao)