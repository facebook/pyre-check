import random
import math

a = input()
b = ''
for _ in range(3):
    c = ''
    for _ in range(4):
        d = ''
        for _ in range(2):
            d += c
            c += b
        b += a
e = d + '.'
f_list = [e for _ in range(3)]
g_list = [f_list for _ in range(6)]
h = random.choice(g_list)
i = random.choice(h)
j = i[0:]
k = (j, j, j)
l, m, n = k
o = l + m + n
p = (o, o, o)
q, r, s = p
t = q + r + s
u_list = [t for _ in range(4)]
v_list = [u_list for _ in range(3)]
w_list = [v_list for _ in range(9)]
x = random.choice(w_list)
y = random.choice(x)
z = random.choice(y)
aa = z + '.'
ab = aa + '6'
ac = ''
counterac = 0
while counterac < 5:
    ac += ab
    counterac += 1
ad = ac[0:]
ae = ad + '1'
af = ae + '8'
ag = f'string {af}'
ah = f'string {ag}'
ai = ''
for _ in range(3):
    for __ in range(5):
                ai += ah
def aj():
    return ai
ak = aj()
al = ''
for _ in range(6):
        if _ == 3:
            break
        al += ak
am = ''
for _ in range(4):
    an = ''
    for _ in range(3):
        ao = ''
        for _ in range(4):
            ao += an
            an += am
        am += al
print(ao)