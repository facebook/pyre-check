import random
import math

a = input()
b = ''
for _ in range(8):
        if _ == 4:
            break
        b += a
c = ''
counterc = 0
while counterc < 4:
    c += b
    counterc += 1
d = (c, c, c)
e, f, g = d
h = e + f + g
i = f'string {h}'
j = (i, i, i)
k, l, m = j
n = k + l + m
def o():
    return n
def p():
    return o()
q = p()
r_list = [q for _ in range(10)]
s = random.choice(r_list)
t = s[0:]
u_list = [t for _ in range(4)]
v = random.choice(u_list)
w = v[0:]
x_set = {w, w, w, w}
x = random.choice(list(x_set))
y = [x for _ in range(6)]
random.shuffle(y)
z = random.choice(y)
def aa():
    return z
ab = aa()
ac = (ab, ab, ab)
ad, ae, af = ac
ag = ad + ae + af
ah = ag + '6'
ai = ah + '1'
aj = f'string {ai}'
def ak():
    return aj
al = ak()
am = ''
for _ in range(2):
    an = ''
    for _ in range(4):
        an += am
        am += al
print(an)