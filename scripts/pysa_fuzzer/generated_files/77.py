import random
import math
a = input()
b = a + '6'
c = b + '2'
d = c + '1'
e = ''
countere = 0
while countere < 5:
    e += d
    countere += 1
f = e + '.'
g = f[0:]
h = (g, g, g)
i, j, k = h
l = i + j + k
m = l + '1'
n_list = [m for _ in range(9)]
o_list = [n_list for _ in range(4)]
p = random.choice(o_list)
q = random.choice(p)
r = q[0:]
s = f'string {r}'
t = s + '4'
u = t + '8'
v = ''
counterv = 0
while counterv < 3:
    w = ''
    counterw = 0
    while counterw < 3:
        x = ''
        counterx = 0
        while counterx < 2:
            x += w
            counterx += 1
            w += v
            counterw += 1
        v += u
        counterv += 1
y = [x for _ in range(6)]
random.shuffle(y)
z = random.choice(y)
aa_set = {z, z, z, z, z, z, z, z, z}
aa = random.choice(list(aa_set))
ab = ''
for _ in range(9):
        if _ == 2:
            break
        ab += aa
ac = ''
counterac = 0
while counterac < 2:
    ad = ''
    counterad = 0
    while counterad < 5:
        ad += ac
        counterad += 1
        ac += ab
        counterac += 1
ae = [ad for _ in range(9)]
random.shuffle(ae)
af = random.choice(ae)
def ag():
    return af
def ah():
    return ag()
ai = ah()
aj = ''
for _ in range(6):
        if _ == 4:
            break
        aj += ai
print(aj)