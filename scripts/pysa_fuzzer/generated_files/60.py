import random
import math
a = input()
if a == '3':
    b = a + ' c1'
elif a == '11':
    b = a + ' c2'
else:
    b = a + ' c3'
c = (b, b, b)
d, e, f = c
g = d + e + f
h = [g for _ in range(7)]
random.shuffle(h)
i = random.choice(h)
j = i + '.'
k_list = [j for _ in range(5)]
l = random.choice(k_list)
m_dict = {59: l, 100: l, 64: l, 67: l, 8: l, 81: l, 33: l}
n = random.choice(list(m_dict.values()))
if n == '2':
    o = n + ' c1'
elif n == '11':
    o = n + ' c2'
else:
    o = n + ' c3'
p = f'string {o}'
q = p[0:]
r = ''
for _ in range(6):
        if _ == 1:
            break
        r += q
s = r + '.'
t_list = [s for _ in range(8)]
u_list = [t_list for _ in range(4)]
v = random.choice(u_list)
w = random.choice(v)
x = w + '.'
y = x + '.'
z = y + '3'
aa = z + '4'
ab = aa + '6'
ac = [ab for _ in range(9)]
random.shuffle(ac)
ad = random.choice(ac)
def ae():
    return ad
def af():
    return ae()
def ag():
    return af()
ah = ag()
if ah == '7':
    ai = ah + ' c1'
elif ah == '14':
    ai = ah + ' c2'
else:
    ai = ah + ' c3'
print(ai)