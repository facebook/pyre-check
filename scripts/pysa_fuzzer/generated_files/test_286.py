import random
import math

a = input()
b = a[0:]
c = b + '3'
d = c + '4'
e = d + '3'
def f():
    return e
g = f()
h = ''
for _ in range(2):
    for __ in range(5):
                h += g
def i():
    return h
def j():
    return i()
def k():
    return j()
l = k()
m = (l, l, l)
n, o, p = m
q = n + o + p
r = [q for _ in range(6)]
random.shuffle(r)
s = random.choice(r)
t = s + '5'
u = t[0:]
v = [u for _ in range(9)]
random.shuffle(v)
w = random.choice(v)
x = ''
for _ in range(10):
        if _ == 2:
            continue
        x += w
y = ''
countery = 0
while countery < 4:
    y += x
    countery += 1
z = y + '.'
aa = [z for _ in range(6)]
random.shuffle(aa)
ab = random.choice(aa)
ac = f'string {ab}'
ad = ac + '.'
ae = ''
for _ in range(5):
    af = ''
    for _ in range(4):
        ag = ''
        for _ in range(5):
            ag += af
            af += ae
        ae += ad
if ag == ag:
    aj = ag + 'c1'
elif ag == '15':
    aj = ah + 'c2'
else:
    aj = ai + 'c3'
print(aj)