import random
import math
a = input()
b = [a for _ in range(7)]
random.shuffle(b)
c = random.choice(b)
d = ''
counterd = 0
while counterd < 2:
    e = ''
    countere = 0
    while countere < 2:
        f = ''
        counterf = 0
        while counterf < 3:
            f += e
            counterf += 1
            e += d
            countere += 1
        d += c
        counterd += 1
g = f[0:]
h = f'string {g}'
i = [h for _ in range(8)]
random.shuffle(i)
j = random.choice(i)
k = ''
counterk = 0
while counterk < 5:
    k += j
    counterk += 1
l = ''
for _ in range(5):
    for __ in range(5):
                l += k
m_set = {l, l}
m = random.choice(list(m_set))
n = f'string {m}'
o_list = [n for _ in range(10)]
p_list = [o_list for _ in range(5)]
q = random.choice(p_list)
r = random.choice(q)
s = (r, r, r)
t, u, v = s
w = t + u + v
x = ''
counterx = 0
while counterx < 2:
    x += w
    counterx += 1
y_dict = {76: x, 53: x, 10: x, 32: x, 89: x, 8: x, 42: x, 42: x}
z_dict = {93: y_dict, 53: y_dict}
aa = random.choice(list(z_dict.values()))
ab = random.choice(list(aa.values()))
ac = f'string {ab}'
ad = ''
counterad = 0
while counterad < 5:
    ae = ''
    counterae = 0
    while counterae < 2:
        ae += ad
        counterae += 1
        ad += ac
        counterad += 1
af_list = [ae for _ in range(7)]
ag_list = [af_list for _ in range(8)]
ah_list = [ag_list for _ in range(9)]
ai = random.choice(ah_list)
aj = random.choice(ai)
ak = random.choice(aj)
al = f'string {ak}'
am = (al, al, al)
an, ao, ap = am
aq = an + ao + ap
print(aq)