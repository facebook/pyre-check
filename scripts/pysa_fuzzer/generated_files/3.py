import random
import math
a = input()
b_list = [a for _ in range(6)]
c = random.choice(b_list)
d = c[0:]
e_set = {d, d, d, d, d, d, d, d, d, d}
e = random.choice(list(e_set))
f = ''
counterf = 0
while counterf < 4:
    f += e
    counterf += 1
g = [f for _ in range(8)]
random.shuffle(g)
h = random.choice(g)
i = (h, h, h)
j, k, l = i
m = j + k + l
n = ''
countern = 0
while countern < 5:
    o = ''
    countero = 0
    while countero < 5:
        o += n
        countero += 1
        n += m
        countern += 1
p = o + '.'
q = f'string {p}'
r = ''
for _ in range(3):
    s = ''
    for _ in range(5):
        t = ''
        for _ in range(2):
            t += s
            s += r
        r += q
u = ''
for _ in range(9):
        if _ == 4:
            continue
        u += t
v = ''
for _ in range(2):
    v += u
if v == '10':
    w = v + ' c1'
elif v == '16':
    w = v + ' c2'
else:
    w = v + ' c3'
x = ''
for _ in range(6):
        if _ == 3:
            break
        x += w
y = ''
for _ in range(2):
    for __ in range(5):
                y += x
z_dict = {91: y, 31: y, 64: y, 82: y, 98: y, 78: y, 96: y, 55: y}
aa = random.choice(list(z_dict.values()))
def ab():
    return aa
def ac():
    return ab()
def ad():
    return ac()
ae = ad()
af_list = [ae for _ in range(3)]
ag_list = [af_list for _ in range(3)]
ah_list = [ag_list for _ in range(4)]
ai = random.choice(ah_list)
aj = random.choice(ai)
ak = random.choice(aj)
print(ak)