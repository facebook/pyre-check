import random
import math
a = input()
b = a + '1'
c = b + '9'
d = c + '6'
e = d + '.'
f = e[0:]
g = f'string {f}'
def h():
    return g
def i():
    return h()
j = i()
k = f'string {j}'
l_list = [k for _ in range(7)]
m_list = [l_list for _ in range(6)]
n_list = [m_list for _ in range(2)]
o = random.choice(n_list)
p = random.choice(o)
q = random.choice(p)
r = [q for _ in range(6)]
random.shuffle(r)
s = random.choice(r)
t = ''
countert = 0
while countert < 5:
    t += s
    countert += 1
u = ''
for _ in range(3):
    for __ in range(2):
                u += t
v = [u for _ in range(8)]
random.shuffle(v)
w = random.choice(v)
x = ''
for _ in range(5):
        if _ == 5:
            break
        x += w
y = [x for _ in range(7)]
random.shuffle(y)
z = random.choice(y)
if z == '7':
    aa = z + ' c1'
elif z == '11':
    aa = z + ' c2'
else:
    aa = z + ' c3'
ab_set = {aa, aa}
ab = random.choice(list(ab_set))
ac = ab + '9'
if ac == '3':
    ad = ac + ' c1'
elif ac == '12':
    ad = ac + ' c2'
else:
    ad = ac + ' c3'
ae = ''
for _ in range(9):
        if _ == 2:
            continue
        ae += ad
print(ae)