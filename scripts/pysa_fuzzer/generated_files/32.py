import random
import math
a = input()
if a == '8':
    b = a + ' c1'
elif a == '16':
    b = a + ' c2'
else:
    b = a + ' c3'
def c():
    return b
d = c()
e = (d, d, d)
f, g, h = e
i = f + g + h
j_set = {i, i, i, i}
j = random.choice(list(j_set))
k = j + '4'
l = k + '8'
m = l + '.'
n = ''
countern = 0
while countern < 2:
    o = ''
    countero = 0
    while countero < 2:
        p = ''
        counterp = 0
        while counterp < 3:
            p += o
            counterp += 1
            o += n
            countero += 1
        n += m
        countern += 1
q = [p for _ in range(9)]
random.shuffle(q)
r = random.choice(q)
def s():
    return r
t = s()
u_list = [t for _ in range(9)]
v = random.choice(u_list)
w = ''
for _ in range(3):
    x = ''
    for _ in range(4):
        x += w
        w += v
y = ''
for _ in range(10):
        if _ == 4:
            break
        y += x
z_set = {y, y}
z = random.choice(list(z_set))
aa = z + '7'
ab = aa + '8'
ac = ab[0:]
ad = ac + '9'
ae = ad + '9'
af_list = [ae for _ in range(3)]
ag_list = [af_list for _ in range(10)]
ah = random.choice(ag_list)
ai = random.choice(ah)
aj = ai[0:]
print(aj)