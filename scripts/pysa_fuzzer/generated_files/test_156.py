import random
import math

a = input()
b_set = {a, a, a, a, a, a, a, a}
b = random.choice(list(b_set))
c = ''
for _ in range(10):
        if _ == 4:
            continue
        c += b
if c == c:
    f = c + 'c1'
elif c == '16':
    f = d + 'c2'
else:
    f = e + 'c3'
if f == f:
    i = f + 'c1'
elif f == '20':
    i = g + 'c2'
else:
    i = h + 'c3'
j = i[0:]
k = ''
for _ in range(5):
    l = ''
    for _ in range(2):
        m = ''
        for _ in range(4):
            m += l
            l += k
        k += j
n_set = {m, m, m, m, m}
n = random.choice(list(n_set))
o_set = {n, n, n, n}
o = random.choice(list(o_set))
p_list = [o for _ in range(3)]
q = random.choice(p_list)
r_set = {q, q, q, q, q, q, q, q, q, q}
r = random.choice(list(r_set))
s = ''
for _ in range(6):
        if _ == 2:
            continue
        s += r
t = s + '.'
u_list = [t for _ in range(6)]
v_list = [u_list for _ in range(5)]
w_list = [v_list for _ in range(8)]
x = random.choice(w_list)
y = random.choice(x)
z = random.choice(y)
aa = (z, z, z)
ab, ac, ad = aa
ae = ab + ac + ad
af = [ae for _ in range(6)]
random.shuffle(af)
ag = random.choice(af)
ah = ag + '6'
ai = ''
counterai = 0
while counterai < 4:
    aj = ''
    counteraj = 0
    while counteraj < 3:
        aj += ai
        counteraj += 1
        ai += ah
        counterai += 1
ak = f'string {aj}'
print(ak)