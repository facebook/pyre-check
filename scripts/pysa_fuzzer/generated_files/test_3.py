import random
import math

a = input()
b = a + '.'
c = ''
for _ in range(10):
        if _ == 2:
            break
        c += b
d = ''
for _ in range(4):
    e = ''
    for _ in range(4):
        f = ''
        for _ in range(3):
            f += e
            e += d
        d += c
g = ''
for _ in range(5):
    g += f
h = g[0:]
i = [h for _ in range(10)]
random.shuffle(i)
j = random.choice(i)
k = ''
for _ in range(4):
    for __ in range(4):
                k += j
l = ''
counterl = 0
while counterl < 3:
    m = ''
    counterm = 0
    while counterm < 3:
        n = ''
        countern = 0
        while countern < 3:
            n += m
            countern += 1
            m += l
            counterm += 1
        l += k
        counterl += 1
if n == n:
    q = n + 'c1'
elif n == '11':
    q = o + 'c2'
else:
    q = p + 'c3'
r = f'string {q}'
s = r + '3'
t_list = [s for _ in range(8)]
u_list = [t_list for _ in range(10)]
v_list = [u_list for _ in range(3)]
w = random.choice(v_list)
x = random.choice(w)
y = random.choice(x)
z_dict = {100: y, 23: y, 65: y, 99: y, 4: y, 90: y, 61: y, 88: y}
aa_dict = {2: z_dict, 34: z_dict, 96: z_dict, 67: z_dict}
ab = random.choice(list(aa_dict.values()))
ac = random.choice(list(ab.values()))
ad = ''
for _ in range(4):
    ae = ''
    for _ in range(2):
        ae += ad
        ad += ac
af = ''
for _ in range(2):
    for __ in range(4):
                af += ae
ag_list = [af for _ in range(3)]
ah_list = [ag_list for _ in range(5)]
ai = random.choice(ah_list)
aj = random.choice(ai)
ak = ''
counterak = 0
while counterak < 3:
    al = ''
    counteral = 0
    while counteral < 2:
        al += ak
        counteral += 1
        ak += aj
        counterak += 1
am = ''
for _ in range(5):
    for __ in range(5):
                am += al
print(am)