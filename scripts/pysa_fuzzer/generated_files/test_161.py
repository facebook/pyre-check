import random
import math

a = input()
def b():
    return a
def c():
    return b()
d = c()
e_list = [d for _ in range(10)]
f_list = [e_list for _ in range(10)]
g = random.choice(f_list)
h = random.choice(g)
i = h + '9'
j = i + '9'
k = ''
for _ in range(5):
    k += j
l = k[0:]
m = ''
for _ in range(5):
    for __ in range(4):
                m += l
n = ''
countern = 0
while countern < 5:
    o = ''
    countero = 0
    while countero < 2:
        o += n
        countero += 1
        n += m
        countern += 1
p = ''
for _ in range(4):
    for __ in range(2):
                p += o
q = [p for _ in range(5)]
random.shuffle(q)
r = random.choice(q)
s = f'string {r}'
t = ''
for _ in range(7):
        if _ == 4:
            break
        t += s
u = t + '.'
v = ''
for _ in range(3):
    w = ''
    for _ in range(5):
        w += v
        v += u
x = [w for _ in range(5)]
random.shuffle(x)
y = random.choice(x)
z_set = {y, y, y, y, y, y}
z = random.choice(list(z_set))
aa = z[0:]
ab = ''
for _ in range(5):
    ac = ''
    for _ in range(4):
        ac += ab
        ab += aa
if ac == ac:
    af = ac + 'c1'
elif ac == '14':
    af = ad + 'c2'
else:
    af = ae + 'c3'
print(af)