import random
import math

a = input()
b = a + '.'
def c():
    return b
def d():
    return c()
e = d()
f = ''
for _ in range(5):
    g = ''
    for _ in range(2):
        h = ''
        for _ in range(5):
            h += g
            g += f
        f += e
i = h + '.'
j = ''
for _ in range(7):
        if _ == 5:
            break
        j += i
k = f'string {j}'
def l():
    return k
def m():
    return l()
n = m()
o = ''
for _ in range(6):
        if _ == 3:
            break
        o += n
p_set = {o, o, o, o, o}
p = random.choice(list(p_set))
q = p + '.'
r = ''
counterr = 0
while counterr < 3:
    r += q
    counterr += 1
s = r + '7'
t = s + '.'
def u():
    return t
def v():
    return u()
w = v()
x = [w for _ in range(10)]
random.shuffle(x)
y = random.choice(x)
z = ''
counterz = 0
while counterz < 2:
    aa = ''
    counteraa = 0
    while counteraa < 4:
        aa += z
        counteraa += 1
        z += y
        counterz += 1
ab = f'string {aa}'
ac = ''
for _ in range(7):
        if _ == 2:
            break
        ac += ab
print(ac)