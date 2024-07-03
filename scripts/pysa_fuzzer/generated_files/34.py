import random
import math
a = input()
if a == '8':
    b = a + ' c1'
elif a == '14':
    b = a + ' c2'
else:
    b = a + ' c3'
def c():
    return b
def d():
    return c()
def e():
    return d()
f = e()
g = f + '5'
h = ''
for _ in range(2):
    i = ''
    for _ in range(5):
        i += h
        h += g
j = ''
for _ in range(5):
    k = ''
    for _ in range(4):
        l = ''
        for _ in range(2):
            l += k
            k += j
        j += i
m = [l for _ in range(5)]
random.shuffle(m)
n = random.choice(m)
def o():
    return n
def p():
    return o()
q = p()
r = ''
for _ in range(10):
        if _ == 5:
            break
        r += q
s = ''
for _ in range(2):
    for __ in range(3):
                s += r
t = ''
for _ in range(2):
    for __ in range(5):
                t += s
if t == '1':
    u = t + ' c1'
elif t == '16':
    u = t + ' c2'
else:
    u = t + ' c3'
v_set = {u, u, u, u, u, u, u, u}
v = random.choice(list(v_set))
w = v[0:]
x = w + '.'
y = ''
for _ in range(4):
    y += x
z = y[0:]
aa = z + '8'
ab = ''
for _ in range(2):
    for __ in range(5):
                ab += aa
print(ab)