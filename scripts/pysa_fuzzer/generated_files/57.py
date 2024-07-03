import random
import math
a = input()
b = a + '.'
c = f'string {b}'
if c == '1':
    d = c + ' c1'
elif c == '11':
    d = c + ' c2'
else:
    d = c + ' c3'
e_set = {d, d, d, d, d, d, d, d, d, d}
e = random.choice(list(e_set))
if e == '10':
    f = e + ' c1'
elif e == '11':
    f = e + ' c2'
else:
    f = e + ' c3'
g = f[0:]
h = f'string {g}'
i = ''
for _ in range(5):
    for __ in range(3):
                i += h
j = f'string {i}'
k = ''
for _ in range(7):
        if _ == 3:
            continue
        k += j
l = ''
for _ in range(5):
    l += k
m = ''
for _ in range(8):
        if _ == 3:
            break
        m += l
def n():
    return m
def o():
    return n()
p = o()
q = p + '7'
r = q + '2'
s = r + '3'
def t():
    return s
def u():
    return t()
v = u()
w = ''
for _ in range(5):
    for __ in range(5):
                w += v
x = ''
for _ in range(5):
    for __ in range(4):
                x += w
y = ''
for _ in range(2):
    for __ in range(2):
                y += x
print(y)