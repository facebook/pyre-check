import random
import math

a = input()
b = ''
for _ in range(5):
    c = ''
    for _ in range(4):
        c += b
        b += a
d = (c, c, c)
e, f, g = d
h = e + f + g
i = h + '7'
j = i + '2'
k = f'string {j}'
l = ''
counterl = 0
while counterl < 4:
    m = ''
    counterm = 0
    while counterm < 2:
        n = ''
        countern = 0
        while countern < 5:
            n += m
            countern += 1
            m += l
            counterm += 1
        l += k
        counterl += 1
o_set = {n, n, n, n, n, n, n}
o = random.choice(list(o_set))
def p():
    return o
def q():
    return p()
def r():
    return q()
s = r()
def t():
    return s
def u():
    return t()
v = u()
w = v[0:]
def x():
    return w
def y():
    return x()
z = y()
aa_dict = {77: z, 87: z, 11: z, 9: z, 22: z}
ab = random.choice(list(aa_dict.values()))
ac = ab + '7'
ad = (ac, ac, ac)
ae, af, ag = ad
ah = ae + af + ag
ai = ''
counterai = 0
while counterai < 4:
    ai += ah
    counterai += 1
aj = ai + '.'
ak = f'string {aj}'
al = ak + '.'
def am():
    return al
def an():
    return am()
ao = an()
print(ao)