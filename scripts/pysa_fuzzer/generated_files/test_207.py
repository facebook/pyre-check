import random
import math

a = input()
b = a + '.'
c = b[0:]
d = (c, c, c)
e, f, g = d
h = e + f + g
i = h + '9'
j = i + '4'
k = ''
for _ in range(6):
        if _ == 1:
            continue
        k += j
l_set = {k, k, k, k, k, k, k, k}
l = random.choice(list(l_set))
m = f'string {l}'
n = ''
for _ in range(7):
        if _ == 4:
            break
        n += m
o = ''
for _ in range(7):
        if _ == 3:
            continue
        o += n
p = o + '2'
q = p + '3'
r = ''
for _ in range(3):
    for __ in range(4):
                r += q
s_set = {r, r, r, r, r, r, r, r}
s = random.choice(list(s_set))
t = [s for _ in range(5)]
random.shuffle(t)
u = random.choice(t)
v_dict = {54: u, 46: u, 38: u, 48: u, 59: u, 96: u}
w = random.choice(list(v_dict.values()))
x = f'string {w}'
y = (x, x, x)
z, aa, ab = y
ac = z + aa + ab
ad_list = [ac for _ in range(6)]
ae_list = [ad_list for _ in range(4)]
af = random.choice(ae_list)
ag = random.choice(af)
ah = ''
for _ in range(4):
    ai = ''
    for _ in range(4):
        ai += ah
        ah += ag
print(ai)