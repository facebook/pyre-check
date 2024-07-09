import random
import math

a = input()
b = ''
counterb = 0
while counterb < 4:
    b += a
    counterb += 1
c = ''
for _ in range(4):
    for __ in range(5):
                c += b
d = f'string {c}'
e_set = {d, d, d, d, d, d, d, d}
e = random.choice(list(e_set))
f = [e for _ in range(8)]
random.shuffle(f)
g = random.choice(f)
h = ''
for _ in range(10):
        if _ == 3:
            break
        h += g
def i():
    return h
j = i()
k = ''
for _ in range(6):
        if _ == 4:
            break
        k += j
l = k + '2'
m = l + '7'
n = m + '7'
o = ''
for _ in range(2):
    o += n
p = [o for _ in range(7)]
random.shuffle(p)
q = random.choice(p)
r = [q for _ in range(5)]
random.shuffle(r)
s = random.choice(r)
t = (s, s, s)
u, v, w = t
x = u + v + w
y_dict = {73: x, 99: x, 98: x, 56: x, 5: x}
z_dict = {42: y_dict, 62: y_dict, 95: y_dict, 82: y_dict, 98: y_dict, 34: y_dict, 96: y_dict, 6: y_dict}
aa = random.choice(list(z_dict.values()))
ab = random.choice(list(aa.values()))
ac = [ab for _ in range(5)]
random.shuffle(ac)
ad = random.choice(ac)
ae = [ad for _ in range(6)]
random.shuffle(ae)
af = random.choice(ae)
ag = af[0:]
ah = f'string {ag}'
print(ah)