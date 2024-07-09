import random
import math

a = input()
b = ''
for _ in range(3):
    for __ in range(5):
                b += a
def c():
    return b
d = c()
e = d + '1'
def f():
    return e
def g():
    return f()
def h():
    return g()
i = h()
j = i + '3'
k = j + '2'
l = ''
for _ in range(5):
    for __ in range(2):
                l += k
m = [l for _ in range(9)]
random.shuffle(m)
n = random.choice(m)
o = n + '2'
p = o + '5'
q = p + '.'
r_set = {q, q, q, q}
r = random.choice(list(r_set))
s_set = {r, r}
s = random.choice(list(s_set))
t_list = [s for _ in range(9)]
u = random.choice(t_list)
def v():
    return u
def w():
    return v()
def x():
    return w()
y = x()
z = y + '.'
aa = ''
for _ in range(8):
        if _ == 2:
            break
        aa += z
ab = (aa, aa, aa)
ac, ad, ae = ab
af = ac + ad + ae
ag = ''
counterag = 0
while counterag < 2:
    ah = ''
    counterah = 0
    while counterah < 4:
        ah += ag
        counterah += 1
        ag += af
        counterag += 1
ai = ah[0:]
print(ai)