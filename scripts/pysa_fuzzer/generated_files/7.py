import random
import math
a = input()
b_list = [a for _ in range(5)]
c = random.choice(b_list)
if c == '7':
    d = c + ' c1'
elif c == '15':
    d = c + ' c2'
else:
    d = c + ' c3'
e = ''
for _ in range(9):
        if _ == 5:
            break
        e += d
f_list = [e for _ in range(4)]
g = random.choice(f_list)
h = ''
for _ in range(8):
        if _ == 4:
            continue
        h += g
i = ''
counteri = 0
while counteri < 2:
    j = ''
    counterj = 0
    while counterj < 4:
        j += i
        counterj += 1
        i += h
        counteri += 1
k = j[0:]
l = ''
for _ in range(3):
    m = ''
    for _ in range(4):
        n = ''
        for _ in range(5):
            n += m
            m += l
        l += k
def o():
    return n
p = o()
if p == '8':
    q = p + ' c1'
elif p == '13':
    q = p + ' c2'
else:
    q = p + ' c3'
r_dict = {77: q, 24: q, 49: q, 10: q, 53: q, 7: q, 55: q, 80: q, 94: q, 33: q}
s_dict = {61: r_dict, 29: r_dict, 96: r_dict, 61: r_dict, 69: r_dict, 31: r_dict, 61: r_dict, 58: r_dict, 56: r_dict}
t = random.choice(list(s_dict.values()))
u = random.choice(list(t.values()))
if u == '2':
    v = u + ' c1'
elif u == '13':
    v = u + ' c2'
else:
    v = u + ' c3'
w = ''
counterw = 0
while counterw < 3:
    x = ''
    counterx = 0
    while counterx < 2:
        x += w
        counterx += 1
        w += v
        counterw += 1
y_dict = {13: x, 73: x, 54: x, 32: x, 73: x}
z = random.choice(list(y_dict.values()))
aa = (z, z, z)
ab, ac, ad = aa
ae = ab + ac + ad
def af():
    return ae
def ag():
    return af()
ah = ag()
ai = (ah, ah, ah)
aj, ak, al = ai
am = aj + ak + al
an = ''
for _ in range(2):
    an += am
print(an)