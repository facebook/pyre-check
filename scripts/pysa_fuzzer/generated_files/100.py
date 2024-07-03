import random
import math
a = input()
b = a + '.'
def c():
    return b
def d():
    return c()
e = d()
f = [e for _ in range(9)]
random.shuffle(f)
g = random.choice(f)
h = [g for _ in range(7)]
random.shuffle(h)
i = random.choice(h)
j_list = [i for _ in range(8)]
k_list = [j_list for _ in range(10)]
l_list = [k_list for _ in range(5)]
m = random.choice(l_list)
n = random.choice(m)
o = random.choice(n)
p = ''
for _ in range(4):
    p += o
q = ''
counterq = 0
while counterq < 4:
    r = ''
    counterr = 0
    while counterr < 5:
        r += q
        counterr += 1
        q += p
        counterq += 1
s = ''
counters = 0
while counters < 2:
    t = ''
    countert = 0
    while countert < 2:
        t += s
        countert += 1
        s += r
        counters += 1
u = t + '.'
v_dict = {44: u, 83: u, 68: u, 87: u, 16: u}
w = random.choice(list(v_dict.values()))
def x():
    return w
y = x()
z_list = [y for _ in range(5)]
aa_list = [z_list for _ in range(5)]
ab = random.choice(aa_list)
ac = random.choice(ab)
ad = ''
for _ in range(5):
    for __ in range(3):
                ad += ac
ae = f'string {ad}'
af_set = {ae, ae}
af = random.choice(list(af_set))
ag_set = {af, af, af, af, af, af, af}
ag = random.choice(list(ag_set))
ah = ''
for _ in range(4):
    for __ in range(4):
                ah += ag
ai_dict = {81: ah, 63: ah, 6: ah, 2: ah, 3: ah, 94: ah, 51: ah, 78: ah, 92: ah, 42: ah}
aj = random.choice(list(ai_dict.values()))
print(aj)