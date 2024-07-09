import random
import math

a = input()
b = ''
counterb = 0
while counterb < 3:
    b += a
    counterb += 1
def c():
    return b
d = c()
def e():
    return d
f = e()
g_set = {f, f, f, f, f, f, f}
g = random.choice(list(g_set))
h = g + '.'
i = f'string {h}'
j = ''
for _ in range(3):
    k = ''
    for _ in range(4):
        k += j
        j += i
l = [k for _ in range(5)]
random.shuffle(l)
m = random.choice(l)
n = m + '.'
o = ''
for _ in range(2):
    for __ in range(2):
                o += n
p = ''
counterp = 0
while counterp < 5:
    q = ''
    counterq = 0
    while counterq < 4:
        q += p
        counterq += 1
        p += o
        counterp += 1
def r():
    return q
s = r()
t_set = {s, s, s}
t = random.choice(list(t_set))
u_set = {t, t, t, t, t, t}
u = random.choice(list(u_set))
if u == u:
    x = u + 'c1'
elif u == '19':
    x = v + 'c2'
else:
    x = w + 'c3'
y = (x, x, x)
z, aa, ab = y
ac = z + aa + ab
ad = ''
counterad = 0
while counterad < 4:
    ad += ac
    counterad += 1
ae = (ad, ad, ad)
af, ag, ah = ae
ai = af + ag + ah
print(ai)