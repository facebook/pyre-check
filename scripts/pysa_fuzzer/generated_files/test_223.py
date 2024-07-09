import random
import math

a = input()
if a == a:
    d = a + 'c1'
elif a == '14':
    d = b + 'c2'
else:
    d = c + 'c3'
def e():
    return d
def f():
    return e()
def g():
    return f()
h = g()
i = ''
for _ in range(2):
    j = ''
    for _ in range(3):
        k = ''
        for _ in range(3):
            k += j
            j += i
        i += h
l = f'string {k}'
m_dict = {9: l, 100: l, 43: l, 66: l}
n_dict = {88: m_dict, 32: m_dict, 14: m_dict, 1: m_dict, 8: m_dict, 40: m_dict, 30: m_dict}
o = random.choice(list(n_dict.values()))
p = random.choice(list(o.values()))
q = (p, p, p)
r, s, t = q
u = r + s + t
v = f'string {u}'
w = v + '.'
x = f'string {w}'
y = ''
for _ in range(2):
    y += x
if y == y:
    ab = y + 'c1'
elif y == '11':
    ab = z + 'c2'
else:
    ab = aa + 'c3'
ac = ab + '1'
ad = ac + '9'
ae = ad + '9'
af = ae[0:]
ag = ''
for _ in range(2):
    ag += af
ah = ''
for _ in range(2):
    for __ in range(3):
                ah += ag
ai = (ah, ah, ah)
aj, ak, al = ai
am = aj + ak + al
an = am + '.'
ao = ''
counterao = 0
while counterao < 4:
    ap = ''
    counterap = 0
    while counterap < 3:
        ap += ao
        counterap += 1
        ao += an
        counterao += 1
print(ap)