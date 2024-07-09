import random
import math

a = input()
b = ''
counterb = 0
while counterb < 5:
    c = ''
    counterc = 0
    while counterc < 3:
        d = ''
        counterd = 0
        while counterd < 3:
            d += c
            counterd += 1
            c += b
            counterc += 1
        b += a
        counterb += 1
e = ''
for _ in range(5):
    for __ in range(3):
                e += d
if e == e:
    h = e + 'c1'
elif e == '17':
    h = f + 'c2'
else:
    h = g + 'c3'
i = h + '.'
j = ''
for _ in range(5):
    for __ in range(2):
                j += i
k = f'string {j}'
l_list = [k for _ in range(6)]
m_list = [l_list for _ in range(8)]
n = random.choice(m_list)
o = random.choice(n)
p = ''
for _ in range(2):
    q = ''
    for _ in range(5):
        q += p
        p += o
r = ''
for _ in range(3):
    r += q
s = (r, r, r)
t, u, v = s
w = t + u + v
x_set = {w, w, w, w, w, w, w, w, w}
x = random.choice(list(x_set))
def y():
    return x
def z():
    return y()
aa = z()
ab = f'string {aa}'
ac = ''
for _ in range(10):
        if _ == 5:
            break
        ac += ab
def ad():
    return ac
def ae():
    return ad()
af = ae()
def ag():
    return af
ah = ag()
ai = [ah for _ in range(7)]
random.shuffle(ai)
aj = random.choice(ai)
ak = (aj, aj, aj)
al, am, an = ak
ao = al + am + an
print(ao)