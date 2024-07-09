import random
import math

a = input()
b = a + '.'
def c():
    return b
def d():
    return c()
def e():
    return d()
f = e()
g = f'string {f}'
h = g + '.'
i = h + '5'
j = i + '5'
k = j + '9'
l = k + '7'
m = ''
for _ in range(10):
        if _ == 5:
            continue
        m += l
n = ''
for _ in range(8):
        if _ == 2:
            continue
        n += m
o = ''
countero = 0
while countero < 3:
    o += n
    countero += 1
p = ''
for _ in range(5):
    q = ''
    for _ in range(5):
        q += p
        p += o
r = q[0:]
s = ''
for _ in range(5):
        if _ == 3:
            continue
        s += r
t = s + '3'
u = t + '6'
v_set = {u, u, u}
v = random.choice(list(v_set))
w = ''
for _ in range(2):
    w += v
def x():
    return w
def y():
    return x()
def z():
    return y()
aa = z()
ab = aa + '.'
ac = ''
for _ in range(9):
        if _ == 5:
            break
        ac += ab
print(ac)