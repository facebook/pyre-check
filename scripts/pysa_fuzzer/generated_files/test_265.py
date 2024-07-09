import random
import math

a = input()
b = [a for _ in range(8)]
random.shuffle(b)
c = random.choice(b)
def d():
    return c
e = d()
def f():
    return e
def g():
    return f()
h = g()
i = h[0:]
j = (i, i, i)
k, l, m = j
n = k + l + m
o = n[0:]
p = o + '1'
q = ''
for _ in range(2):
    q += p
def r():
    return q
def s():
    return r()
t = s()
u = t + '8'
v = u[0:]
w = ''
for _ in range(5):
        if _ == 4:
            continue
        w += v
x_dict = {97: w, 75: w, 81: w, 92: w, 54: w, 86: w, 20: w}
y_dict = {72: x_dict, 39: x_dict, 85: x_dict, 29: x_dict}
z = random.choice(list(y_dict.values()))
aa = random.choice(list(z.values()))
ab = [aa for _ in range(5)]
random.shuffle(ab)
ac = random.choice(ab)
ad = ''
for _ in range(2):
    for __ in range(2):
                ad += ac
ae = ''
for _ in range(4):
    for __ in range(3):
                ae += ad
af = f'string {ae}'
ag = [af for _ in range(8)]
random.shuffle(ag)
ah = random.choice(ag)
print(ah)