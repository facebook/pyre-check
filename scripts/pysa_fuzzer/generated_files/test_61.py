import random
import math

a = input()
b_dict = {64: a, 100: a, 94: a, 84: a, 83: a, 97: a, 18: a, 5: a}
c_dict = {71: b_dict, 11: b_dict, 14: b_dict, 80: b_dict, 88: b_dict, 17: b_dict, 4: b_dict, 5: b_dict, 32: b_dict, 65: b_dict}
d_dict = {32: c_dict, 71: c_dict, 17: c_dict, 37: c_dict, 47: c_dict, 44: c_dict, 21: c_dict, 42: c_dict}
e = random.choice(list(d_dict.values()))
f = random.choice(list(e.values()))
g = random.choice(list(f.values()))
h = ''
counterh = 0
while counterh < 2:
    i = ''
    counteri = 0
    while counteri < 4:
        j = ''
        counterj = 0
        while counterj < 3:
            j += i
            counterj += 1
            i += h
            counteri += 1
        h += g
        counterh += 1
k_set = {j, j, j, j, j, j, j}
k = random.choice(list(k_set))
l = k[0:]
m = ''
for _ in range(3):
    n = ''
    for _ in range(5):
        n += m
        m += l
o_dict = {21: n, 93: n, 30: n, 40: n, 52: n, 71: n, 51: n, 100: n, 81: n, 53: n}
p = random.choice(list(o_dict.values()))
q = ''
for _ in range(5):
    q += p
r = f'string {q}'
s = r + '.'
t = ''
countert = 0
while countert < 3:
    t += s
    countert += 1
u = t + '9'
v = u + '5'
w = ''
counterw = 0
while counterw < 3:
    x = ''
    counterx = 0
    while counterx < 2:
        y = ''
        countery = 0
        while countery < 3:
            y += x
            countery += 1
            x += w
            counterx += 1
        w += v
        counterw += 1
z = y + '1'
aa = z + '1'
ab = aa[0:]
ac_set = {ab, ab, ab}
ac = random.choice(list(ac_set))
ad = [ac for _ in range(7)]
random.shuffle(ad)
ae = random.choice(ad)
af = ae + '6'
ag = af + '3'
ah = ''
for _ in range(2):
    for __ in range(3):
                ah += ag
print(ah)