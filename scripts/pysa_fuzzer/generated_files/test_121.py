import random
import math

a = input()
def b():
    return a
c = b()
d = c + '7'
e = d + '1'
f = e + '4'
g = f + '4'
h = f'string {g}'
i_list = [h for _ in range(10)]
j_list = [i_list for _ in range(5)]
k = random.choice(j_list)
l = random.choice(k)
m = ''
counterm = 0
while counterm < 4:
    n = ''
    countern = 0
    while countern < 5:
        o = ''
        countero = 0
        while countero < 4:
            o += n
            countero += 1
            n += m
            countern += 1
        m += l
        counterm += 1
p = ''
for _ in range(5):
        if _ == 4:
            continue
        p += o
q = p + '7'
r_list = [q for _ in range(3)]
s_list = [r_list for _ in range(4)]
t_list = [s_list for _ in range(2)]
u = random.choice(t_list)
v = random.choice(u)
w = random.choice(v)
x = [w for _ in range(5)]
random.shuffle(x)
y = random.choice(x)
z_set = {y, y, y, y, y, y}
z = random.choice(list(z_set))
def aa():
    return z
def ab():
    return aa()
ac = ab()
ad_set = {ac, ac, ac, ac, ac, ac, ac, ac}
ad = random.choice(list(ad_set))
ae_list = [ad for _ in range(7)]
af_list = [ae_list for _ in range(3)]
ag_list = [af_list for _ in range(5)]
ah = random.choice(ag_list)
ai = random.choice(ah)
aj = random.choice(ai)
ak = ''
for _ in range(4):
    ak += aj
al = ''
for _ in range(5):
        if _ == 2:
            continue
        al += ak
am = (al, al, al)
an, ao, ap = am
aq = an + ao + ap
ar = ''
for _ in range(4):
    at = ''
    for _ in range(4):
        at += ar
        ar += aq
print(at)