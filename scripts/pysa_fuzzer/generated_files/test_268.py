import random
import math

a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g = ''
for _ in range(2):
    h = ''
    for _ in range(3):
        i = ''
        for _ in range(5):
            i += h
            h += g
        g += f
j = ''
for _ in range(9):
        if _ == 1:
            break
        j += i
k = f'string {j}'
l = k + '8'
m = ''
for _ in range(5):
        if _ == 3:
            break
        m += l
n = m + '1'
o = n + '.'
p_list = [o for _ in range(2)]
q = random.choice(p_list)
r = [q for _ in range(7)]
random.shuffle(r)
s = random.choice(r)
t_list = [s for _ in range(5)]
u = random.choice(t_list)
v = f'string {u}'
w = [v for _ in range(7)]
random.shuffle(w)
x = random.choice(w)
y = ''
countery = 0
while countery < 2:
    y += x
    countery += 1
z_set = {y, y, y, y, y}
z = random.choice(list(z_set))
aa_list = [z for _ in range(6)]
ab_list = [aa_list for _ in range(2)]
ac = random.choice(ab_list)
ad = random.choice(ac)
ae = ''
counterae = 0
while counterae < 3:
    ae += ad
    counterae += 1
af = ''
counteraf = 0
while counteraf < 2:
    af += ae
    counteraf += 1
print(af)