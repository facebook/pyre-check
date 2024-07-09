import random
import math

a = input()
b = [a for _ in range(5)]
random.shuffle(b)
c = random.choice(b)
d = c + '.'
def e():
    return d
def f():
    return e()
def g():
    return f()
h = g()
i_set = {h, h, h, h, h, h, h, h, h, h}
i = random.choice(list(i_set))
j = [i for _ in range(6)]
random.shuffle(j)
k = random.choice(j)
l = (k, k, k)
m, n, o = l
p = m + n + o
q = (p, p, p)
r, s, t = q
u = r + s + t
def v():
    return u
def w():
    return v()
x = w()
y_set = {x, x}
y = random.choice(list(y_set))
z = ''
for _ in range(10):
        if _ == 4:
            continue
        z += y
aa_list = [z for _ in range(7)]
ab_list = [aa_list for _ in range(5)]
ac_list = [ab_list for _ in range(3)]
ad = random.choice(ac_list)
ae = random.choice(ad)
af = random.choice(ae)
ag = af + '7'
ah = ag + '7'
ai = f'string {ah}'
aj = f'string {ai}'
ak = ''
for _ in range(6):
        if _ == 3:
            break
        ak += aj
al = ak[0:]
am = ''
for _ in range(2):
    am += al
an = am[0:]
print(an)