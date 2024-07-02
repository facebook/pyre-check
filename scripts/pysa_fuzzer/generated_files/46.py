import random
import math
a = input()
b = ''
for _ in range(4):
    c = ''
    for _ in range(5):
        c += b
        b += a
d_list = [c for _ in range(6)]
e_list = [d_list for _ in range(2)]
f_list = [e_list for _ in range(2)]
g = random.choice(f_list)
h = random.choice(g)
i = random.choice(h)
j = [i for _ in range(8)]
random.shuffle(j)
k = random.choice(j)
l = k + '.'
m = ''
for _ in range(7):
        if _ == 1:
            continue
        m += l
n = ''
for _ in range(5):
    for __ in range(4):
                n += m
o = ''
countero = 0
while countero < 5:
    o += n
    countero += 1
p = o[0:]
q = p + '.'
r = f'string {q}'
s = r + '2'
t = s + '7'
u = t + '4'
v = u[0:]
w_dict = {29: v, 38: v, 81: v, 86: v, 90: v, 50: v}
x_dict = {3: w_dict, 48: w_dict, 95: w_dict, 9: w_dict, 77: w_dict}
y = random.choice(list(x_dict.values()))
z = random.choice(list(y.values()))
aa = z[0:]
ab = aa + '9'
ac = ab + '8'
ad_set = {ac, ac, ac, ac, ac, ac, ac, ac, ac}
ad = random.choice(list(ad_set))
ae = f'string {ad}'
af_set = {ae, ae, ae, ae, ae}
af = random.choice(list(af_set))
print(af)