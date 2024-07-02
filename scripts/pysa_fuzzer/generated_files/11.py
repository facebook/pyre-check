import random
import math
a = input()
b = [a for _ in range(8)]
random.shuffle(b)
c = random.choice(b)
d = c + '.'
e = ''
countere = 0
while countere < 4:
    f = ''
    counterf = 0
    while counterf < 4:
        f += e
        counterf += 1
        e += d
        countere += 1
g_set = {f, f, f, f, f, f}
g = random.choice(list(g_set))
if g == '4':
    h = g + ' c1'
elif g == '11':
    h = g + ' c2'
else:
    h = g + ' c3'
if h == '4':
    i = h + ' c1'
elif h == '13':
    i = h + ' c2'
else:
    i = h + ' c3'
j = i[0:]
k_list = [j for _ in range(10)]
l_list = [k_list for _ in range(2)]
m_list = [l_list for _ in range(6)]
n = random.choice(m_list)
o = random.choice(n)
p = random.choice(o)
if p == '3':
    q = p + ' c1'
elif p == '12':
    q = p + ' c2'
else:
    q = p + ' c3'
r = f'string {q}'
s_dict = {47: r, 52: r, 24: r, 20: r}
t = random.choice(list(s_dict.values()))
u = ''
for _ in range(2):
    u += t
v = (u, u, u)
w, x, y = v
z = w + x + y
aa = ''
for _ in range(7):
        if _ == 3:
            break
        aa += z
ab_set = {aa, aa, aa}
ab = random.choice(list(ab_set))
def ac():
    return ab
def ad():
    return ac()
ae = ad()
af = f'string {ae}'
ag = af + '4'
print(ag)