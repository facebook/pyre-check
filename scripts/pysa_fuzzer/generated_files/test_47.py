import random
import math

a = input()
b = ''
for _ in range(9):
        if _ == 4:
            break
        b += a
c_set = {b, b, b, b}
c = random.choice(list(c_set))
if c == c:
    f = c + 'c1'
elif c == '15':
    f = d + 'c2'
else:
    f = e + 'c3'
g = f[0:]
h = ''
counterh = 0
while counterh < 3:
    h += g
    counterh += 1
i_set = {h, h, h}
i = random.choice(list(i_set))
j = ''
for _ in range(4):
    for __ in range(2):
                j += i
k = j + '8'
l = k + '5'
m = ''
for _ in range(9):
        if _ == 4:
            break
        m += l
n = ''
countern = 0
while countern < 2:
    n += m
    countern += 1
o = [n for _ in range(10)]
random.shuffle(o)
p = random.choice(o)
q = ''
for _ in range(4):
    for __ in range(3):
                q += p
r = q[0:]
s_list = [r for _ in range(8)]
t_list = [s_list for _ in range(6)]
u = random.choice(t_list)
v = random.choice(u)
w = [v for _ in range(7)]
random.shuffle(w)
x = random.choice(w)
y = x + '4'
z = y + '1'
aa = ''
for _ in range(3):
    for __ in range(5):
                aa += z
ab = aa[0:]
print(ab)