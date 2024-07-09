import random
import math

a = input()
b = a[0:]
c = f'string {b}'
d = c + '.'
e = ''
for _ in range(3):
    for __ in range(2):
                e += d
f = ''
for _ in range(6):
        if _ == 1:
            break
        f += e
g = [f for _ in range(6)]
random.shuffle(g)
h = random.choice(g)
i = ''
counteri = 0
while counteri < 5:
    j = ''
    counterj = 0
    while counterj < 3:
        j += i
        counterj += 1
        i += h
        counteri += 1
k = ''
for _ in range(9):
        if _ == 1:
            break
        k += j
l = k[0:]
m = ''
counterm = 0
while counterm < 4:
    m += l
    counterm += 1
n = ''
for _ in range(3):
    for __ in range(3):
                n += m
if n == n:
    q = n + 'c1'
elif n == '15':
    q = o + 'c2'
else:
    q = p + 'c3'
r = ''
for _ in range(5):
        if _ == 3:
            break
        r += q
s_dict = {30: r, 37: r, 15: r, 65: r, 14: r, 47: r, 55: r, 31: r, 37: r}
t_dict = {99: s_dict, 30: s_dict, 4: s_dict, 41: s_dict, 18: s_dict, 51: s_dict, 46: s_dict, 30: s_dict, 21: s_dict, 34: s_dict}
u_dict = {32: t_dict, 43: t_dict, 86: t_dict, 79: t_dict, 34: t_dict, 52: t_dict, 9: t_dict, 37: t_dict}
v = random.choice(list(u_dict.values()))
w = random.choice(list(v.values()))
x = random.choice(list(w.values()))
y = x[0:]
z = ''
counterz = 0
while counterz < 2:
    z += y
    counterz += 1
aa_list = [z for _ in range(9)]
ab_list = [aa_list for _ in range(2)]
ac_list = [ab_list for _ in range(9)]
ad = random.choice(ac_list)
ae = random.choice(ad)
af = random.choice(ae)
ag = [af for _ in range(8)]
random.shuffle(ag)
ah = random.choice(ag)
print(ah)