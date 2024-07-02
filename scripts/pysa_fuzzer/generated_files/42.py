import random
import math
a = input()
if a == '9':
    b = a + ' c1'
elif a == '16':
    b = a + ' c2'
else:
    b = a + ' c3'
c_set = {b, b, b, b, b, b, b, b, b, b}
c = random.choice(list(c_set))
d = (c, c, c)
e, f, g = d
h = e + f + g
i = h[0:]
j = ''
for _ in range(10):
        if _ == 5:
            continue
        j += i
k = j + '.'
l = ''
for _ in range(6):
        if _ == 1:
            break
        l += k
m = ''
for _ in range(3):
    for __ in range(5):
                m += l
n = m[0:]
o_set = {n, n, n, n, n}
o = random.choice(list(o_set))
p_list = [o for _ in range(2)]
q = random.choice(p_list)
r = ''
counterr = 0
while counterr < 4:
    r += q
    counterr += 1
s = [r for _ in range(5)]
random.shuffle(s)
t = random.choice(s)
u = t[0:]
if u == '5':
    v = u + ' c1'
elif u == '13':
    v = u + ' c2'
else:
    v = u + ' c3'
w = v + '6'
x = ''
counterx = 0
while counterx < 5:
    x += w
    counterx += 1
y_set = {x, x, x, x, x, x}
y = random.choice(list(y_set))
print(y)