import random
import math

a = input()
def b():
    return a
c = b()
d_set = {c, c, c}
d = random.choice(list(d_set))
e_set = {d, d, d, d, d}
e = random.choice(list(e_set))
f = e + '.'
g = f + '4'
h_set = {g, g, g, g, g, g, g, g}
h = random.choice(list(h_set))
i_list = [h for _ in range(4)]
j_list = [i_list for _ in range(8)]
k_list = [j_list for _ in range(8)]
l = random.choice(k_list)
m = random.choice(l)
n = random.choice(m)
o = [n for _ in range(10)]
random.shuffle(o)
p = random.choice(o)
q_set = {p, p, p, p, p, p, p, p, p, p}
q = random.choice(list(q_set))
r = q[0:]
s = r + '.'
t = s + '1'
u = t + '8'
v = ''
counterv = 0
while counterv < 3:
    w = ''
    counterw = 0
    while counterw < 2:
        w += v
        counterw += 1
        v += u
        counterv += 1
x = [w for _ in range(9)]
random.shuffle(x)
y = random.choice(x)
z = ''
for _ in range(10):
        if _ == 4:
            break
        z += y
if z == z:
    ac = z + 'c1'
elif z == '14':
    ac = aa + 'c2'
else:
    ac = ab + 'c3'
ad = ''
for _ in range(5):
        if _ == 4:
            continue
        ad += ac
ae_dict = {38: ad, 46: ad, 30: ad, 28: ad, 16: ad, 79: ad, 49: ad}
af = random.choice(list(ae_dict.values()))
print(af)