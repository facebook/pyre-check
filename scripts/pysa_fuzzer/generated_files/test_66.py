import random
import math

a = input()
if a == a:
    d = a + 'c1'
elif a == '20':
    d = b + 'c2'
else:
    d = c + 'c3'
e = ''
for _ in range(4):
    e += d
f = [e for _ in range(5)]
random.shuffle(f)
g = random.choice(f)
h = (g, g, g)
i, j, k = h
l = i + j + k
m = l + '.'
n = m[0:]
if n == n:
    q = n + 'c1'
elif n == '15':
    q = o + 'c2'
else:
    q = p + 'c3'
def r():
    return q
def s():
    return r()
def t():
    return s()
u = t()
v = (u, u, u)
w, x, y = v
z = w + x + y
aa = [z for _ in range(10)]
random.shuffle(aa)
ab = random.choice(aa)
ac = (ab, ab, ab)
ad, ae, af = ac
ag = ad + ae + af
ah = ''
for _ in range(10):
        if _ == 5:
            continue
        ah += ag
ai = ''
counterai = 0
while counterai < 3:
    aj = ''
    counteraj = 0
    while counteraj < 3:
        ak = ''
        counterak = 0
        while counterak < 3:
            ak += aj
            counterak += 1
            aj += ai
            counteraj += 1
        ai += ah
        counterai += 1
al = ak + '2'
am = [al for _ in range(5)]
random.shuffle(am)
an = random.choice(am)
ao_list = [an for _ in range(2)]
ap = random.choice(ao_list)
aq = (ap, ap, ap)
ar, at, au = aq
av = ar + at + au
aw = ''
for _ in range(7):
        if _ == 5:
            break
        aw += av
print(aw)