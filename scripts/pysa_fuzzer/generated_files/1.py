import random
import math
a = input()
if a == '4':
    b = a + ' c1'
elif a == '14':
    b = a + ' c2'
else:
    b = a + ' c3'
c = b[0:]
d = c + '9'
e = ''
for _ in range(9):
        if _ == 1:
            break
        e += d
f = e[0:]
g_list = [f for _ in range(7)]
h_list = [g_list for _ in range(8)]
i_list = [h_list for _ in range(10)]
j = random.choice(i_list)
k = random.choice(j)
l = random.choice(k)
m_dict = {84: l, 70: l}
n = random.choice(list(m_dict.values()))
o = ''
for _ in range(8):
        if _ == 4:
            continue
        o += n
p = o[0:]
q_list = [p for _ in range(9)]
r_list = [q_list for _ in range(5)]
s_list = [r_list for _ in range(5)]
t = random.choice(s_list)
u = random.choice(t)
v = random.choice(u)
w = v[0:]
x = w + '.'
y_set = {x, x, x, x, x, x, x, x, x, x}
y = random.choice(list(y_set))
z = ''
counterz = 0
while counterz < 5:
    z += y
    counterz += 1
aa = f'string {z}'
ab = aa + '.'
def ac():
    return ab
def ad():
    return ac()
def ae():
    return ad()
af = ae()
ag = ''
for _ in range(8):
        if _ == 3:
            break
        ag += af
print(ag)