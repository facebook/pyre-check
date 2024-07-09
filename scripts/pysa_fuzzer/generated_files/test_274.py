import random
import math

a = input()
b = ''
for _ in range(7):
        if _ == 2:
            break
        b += a
c_list = [b for _ in range(4)]
d_list = [c_list for _ in range(9)]
e_list = [d_list for _ in range(10)]
f = random.choice(e_list)
g = random.choice(f)
h = random.choice(g)
i_set = {h, h, h, h, h, h, h}
i = random.choice(list(i_set))
j = f'string {i}'
k = [j for _ in range(10)]
random.shuffle(k)
l = random.choice(k)
m = f'string {l}'
n = m + '.'
o = ''
for _ in range(9):
        if _ == 2:
            continue
        o += n
p = ''
counterp = 0
while counterp < 2:
    p += o
    counterp += 1
q = ''
for _ in range(4):
    for __ in range(2):
                q += p
if q == q:
    t = q + 'c1'
elif q == '13':
    t = r + 'c2'
else:
    t = s + 'c3'
u = ''
for _ in range(10):
        if _ == 5:
            break
        u += t
v = ''
for _ in range(5):
        if _ == 3:
            break
        v += u
w = [v for _ in range(8)]
random.shuffle(w)
x = random.choice(w)
y = f'string {x}'
z = ''
for _ in range(2):
    aa = ''
    for _ in range(3):
        ab = ''
        for _ in range(5):
            ab += aa
            aa += z
        z += y
ac = ab + '8'
def ad():
    return ac
def ae():
    return ad()
af = ae()
print(af)