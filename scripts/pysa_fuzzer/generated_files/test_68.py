import random
import math

a = input()
b = f'string {a}'
c = b[0:]
d = f'string {c}'
e = (d, d, d)
f, g, h = e
i = f + g + h
j = ''
for _ in range(4):
    for __ in range(4):
                j += i
k = f'string {j}'
l = ''
counterl = 0
while counterl < 3:
    m = ''
    counterm = 0
    while counterm < 2:
        m += l
        counterm += 1
        l += k
        counterl += 1
n = m + '.'
o = n[0:]
p = ''
for _ in range(3):
    for __ in range(3):
                p += o
q = ''
for _ in range(2):
    for __ in range(3):
                q += p
r = ''
for _ in range(5):
    for __ in range(4):
                r += q
s = [r for _ in range(6)]
random.shuffle(s)
t = random.choice(s)
u_list = [t for _ in range(4)]
v = random.choice(u_list)
w_dict = {66: v, 13: v, 42: v, 88: v}
x_dict = {41: w_dict, 46: w_dict, 87: w_dict, 53: w_dict, 41: w_dict, 62: w_dict}
y_dict = {94: x_dict, 95: x_dict, 51: x_dict, 36: x_dict, 5: x_dict}
z = random.choice(list(y_dict.values()))
aa = random.choice(list(z.values()))
ab = random.choice(list(aa.values()))
ac = ''
for _ in range(4):
    ac += ab
ad = ac[0:]
ae = ''
for _ in range(6):
        if _ == 4:
            continue
        ae += ad
print(ae)