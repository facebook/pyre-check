import random
import math

a = input()
b = a + '5'
c = b + '5'
d = c + '1'
e = ''
countere = 0
while countere < 3:
    f = ''
    counterf = 0
    while counterf < 4:
        g = ''
        counterg = 0
        while counterg < 5:
            g += f
            counterg += 1
            f += e
            counterf += 1
        e += d
        countere += 1
h = ''
for _ in range(3):
    i = ''
    for _ in range(5):
        j = ''
        for _ in range(2):
            j += i
            i += h
        h += g
k = j + '3'
l = k + '3'
m = l + '.'
n_set = {m, m, m, m, m, m, m}
n = random.choice(list(n_set))
o = ''
for _ in range(4):
    o += n
p_set = {o, o, o, o, o, o, o, o, o, o}
p = random.choice(list(p_set))
q = (p, p, p)
r, s, t = q
u = r + s + t
def v():
    return u
def w():
    return v()
x = w()
y = f'string {x}'
z_list = [y for _ in range(7)]
aa = random.choice(z_list)
ab_set = {aa, aa, aa, aa, aa, aa}
ab = random.choice(list(ab_set))
ac = ab + '1'
ad = ac + '5'
def ae():
    return ad
def af():
    return ae()
ag = af()
ah = f'string {ag}'
ai = ah + '5'
aj = [ai for _ in range(6)]
random.shuffle(aj)
ak = random.choice(aj)
print(ak)