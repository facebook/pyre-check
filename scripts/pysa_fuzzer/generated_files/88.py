import random
import math
a = input()
b_dict = {89: a, 33: a, 49: a, 46: a, 40: a, 69: a, 19: a}
c = random.choice(list(b_dict.values()))
if c == '8':
    d = c + ' c1'
elif c == '15':
    d = c + ' c2'
else:
    d = c + ' c3'
if d == '5':
    e = d + ' c1'
elif d == '15':
    e = d + ' c2'
else:
    e = d + ' c3'
f_dict = {76: e, 54: e, 34: e, 40: e}
g = random.choice(list(f_dict.values()))
h = ''
counterh = 0
while counterh < 4:
    i = ''
    counteri = 0
    while counteri < 5:
        j = ''
        counterj = 0
        while counterj < 2:
            j += i
            counterj += 1
            i += h
            counteri += 1
        h += g
        counterh += 1
k = ''
for _ in range(10):
        if _ == 2:
            break
        k += j
def l():
    return k
def m():
    return l()
n = m()
o = [n for _ in range(10)]
random.shuffle(o)
p = random.choice(o)
q = p + '.'
r = f'string {q}'
s = ''
for _ in range(10):
        if _ == 1:
            break
        s += r
t = ''
for _ in range(3):
    u = ''
    for _ in range(5):
        v = ''
        for _ in range(3):
            v += u
            u += t
        t += s
if v == '9':
    w = v + ' c1'
elif v == '14':
    w = v + ' c2'
else:
    w = v + ' c3'
x = w + '6'
y = x + '3'
z = ''
for _ in range(3):
    for __ in range(4):
                z += y
aa_list = [z for _ in range(9)]
ab = random.choice(aa_list)
ac = ''
for _ in range(9):
        if _ == 4:
            break
        ac += ab
if ac == '5':
    ad = ac + ' c1'
elif ac == '11':
    ad = ac + ' c2'
else:
    ad = ac + ' c3'
print(ad)