import random
import math

a = input()
b = ''
for _ in range(2):
    for __ in range(4):
                b += a
c = ''
for _ in range(4):
    for __ in range(3):
                c += b
d = c + '4'
e = d + '6'
def f():
    return e
def g():
    return f()
def h():
    return g()
i = h()
j = f'string {i}'
k = ''
counterk = 0
while counterk < 4:
    l = ''
    counterl = 0
    while counterl < 3:
        l += k
        counterl += 1
        k += j
        counterk += 1
m = l + '.'
n_dict = {25: m, 33: m, 30: m, 1: m}
o = random.choice(list(n_dict.values()))
p = o + '.'
q = f'string {p}'
r = ''
for _ in range(6):
        if _ == 5:
            break
        r += q
s = [r for _ in range(5)]
random.shuffle(s)
t = random.choice(s)
u = f'string {t}'
v = u + '9'
w = v + '5'
x = w + '7'
y = f'string {x}'
z = ''
for _ in range(3):
    for __ in range(2):
                z += y
aa = ''
for _ in range(3):
    ab = ''
    for _ in range(5):
        ab += aa
        aa += z
ac = [ab for _ in range(8)]
random.shuffle(ac)
ad = random.choice(ac)
print(ad)