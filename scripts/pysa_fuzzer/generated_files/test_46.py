import random
import math

a = input()
b = f'string {a}'
c = ''
for _ in range(2):
    d = ''
    for _ in range(2):
        d += c
        c += b
e = d + '2'
f = e[0:]
g = ''
for _ in range(4):
    h = ''
    for _ in range(2):
        h += g
        g += f
i = h + '.'
j = ''
for _ in range(4):
    for __ in range(2):
                j += i
if j == j:
    m = j + 'c1'
elif j == '18':
    m = k + 'c2'
else:
    m = l + 'c3'
n_set = {m, m, m, m, m}
n = random.choice(list(n_set))
o = n + '8'
p = o + '9'
q = p + '1'
r_set = {q, q}
r = random.choice(list(r_set))
s_list = [r for _ in range(10)]
t_list = [s_list for _ in range(5)]
u = random.choice(t_list)
v = random.choice(u)
w_set = {v, v, v, v, v, v, v, v, v}
w = random.choice(list(w_set))
x = ''
for _ in range(9):
        if _ == 5:
            continue
        x += w
y = ''
for _ in range(7):
        if _ == 2:
            break
        y += x
def z():
    return y
aa = z()
ab = ''
counterab = 0
while counterab < 5:
    ac = ''
    counterac = 0
    while counterac < 4:
        ad = ''
        counterad = 0
        while counterad < 5:
            ad += ac
            counterad += 1
            ac += ab
            counterac += 1
        ab += aa
        counterab += 1
print(ad)