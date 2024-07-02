import random
import math
a = input()
b = [a for _ in range(8)]
random.shuffle(b)
c = random.choice(b)
d = (c, c, c)
e, f, g = d
h = e + f + g
i_list = [h for _ in range(6)]
j_list = [i_list for _ in range(2)]
k = random.choice(j_list)
l = random.choice(k)
m = f'string {l}'
n = [m for _ in range(8)]
random.shuffle(n)
o = random.choice(n)
p = ''
for _ in range(4):
    q = ''
    for _ in range(5):
        r = ''
        for _ in range(3):
            r += q
            q += p
        p += o
def s():
    return r
def t():
    return s()
u = t()
v = u + '5'
w = v + '2'
if w == '8':
    x = w + ' c1'
elif w == '19':
    x = w + ' c2'
else:
    x = w + ' c3'
def y():
    return x
z = y()
aa_set = {z, z, z, z, z, z, z, z, z}
aa = random.choice(list(aa_set))
ab = ''
for _ in range(8):
        if _ == 5:
            continue
        ab += aa
ac = ''
for _ in range(2):
    for __ in range(3):
                ac += ab
ad = ''
for _ in range(5):
    for __ in range(4):
                ad += ac
ae = ''
for _ in range(4):
    ae += ad
if ae == '7':
    af = ae + ' c1'
elif ae == '15':
    af = ae + ' c2'
else:
    af = ae + ' c3'
ag = ''
counterag = 0
while counterag < 5:
    ag += af
    counterag += 1
ah_list = [ag for _ in range(4)]
ai_list = [ah_list for _ in range(9)]
aj_list = [ai_list for _ in range(9)]
ak = random.choice(aj_list)
al = random.choice(ak)
am = random.choice(al)
print(am)