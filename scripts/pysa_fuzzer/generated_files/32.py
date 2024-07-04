import random
import math
a = input()
if a == '6':
    b = a + ' c1'
elif a == '17':
    b = a + ' c2'
else:
    b = a + ' c3'
c = (b, b, b)
d, e, f = c
g = d + e + f
h = ''
for _ in range(7):
        if _ == 4:
            break
        h += g
if h == '5':
    i = h + ' c1'
elif h == '13':
    i = h + ' c2'
else:
    i = h + ' c3'
j = (i, i, i)
k, l, m = j
n = k + l + m
o = ''
for _ in range(2):
    p = ''
    for _ in range(3):
        q = ''
        for _ in range(3):
            q += p
            p += o
        o += n
r = ''
for _ in range(3):
    s = ''
    for _ in range(2):
        t = ''
        for _ in range(5):
            t += s
            s += r
        r += q
u = [t for _ in range(10)]
random.shuffle(u)
v = random.choice(u)
w = v + '.'
x_dict = {94: w, 64: w, 46: w, 69: w, 47: w, 98: w, 44: w}
y_dict = {10: x_dict, 82: x_dict, 13: x_dict, 57: x_dict, 48: x_dict, 91: x_dict, 57: x_dict, 22: x_dict, 47: x_dict, 95: x_dict}
z = random.choice(list(y_dict.values()))
aa = random.choice(list(z.values()))
ab = f'string {aa}'
ac = ab + '.'
def ad():
    return ac
def ae():
    return ad()
af = ae()
ag = af + '.'
ah = ''
for _ in range(5):
    for __ in range(4):
                ah += ag
if ah == '8':
    ai = ah + ' c1'
elif ah == '18':
    ai = ah + ' c2'
else:
    ai = ah + ' c3'
def aj():
    return ai
ak = aj()
al = ak[0:]
print(al)