import random
import math
a = input()
b = ''
counterb = 0
while counterb < 5:
    c = ''
    counterc = 0
    while counterc < 3:
        c += b
        counterc += 1
        b += a
        counterb += 1
d_set = {c, c, c, c, c, c, c, c}
d = random.choice(list(d_set))
e = d + '.'
f = [e for _ in range(8)]
random.shuffle(f)
g = random.choice(f)
h = g + '.'
i_dict = {73: h, 66: h, 13: h, 94: h, 5: h, 90: h, 77: h}
j_dict = {4: i_dict, 11: i_dict, 46: i_dict, 9: i_dict, 62: i_dict, 76: i_dict, 80: i_dict, 96: i_dict, 5: i_dict}
k = random.choice(list(j_dict.values()))
l = random.choice(list(k.values()))
m = ''
counterm = 0
while counterm < 4:
    n = ''
    countern = 0
    while countern < 4:
        o = ''
        countero = 0
        while countero < 3:
            o += n
            countero += 1
            n += m
            countern += 1
        m += l
        counterm += 1
p = o[0:]
q = ''
for _ in range(3):
    r = ''
    for _ in range(3):
        s = ''
        for _ in range(4):
            s += r
            r += q
        q += p
t = f'string {s}'
if t == '9':
    u = t + ' c1'
elif t == '17':
    u = t + ' c2'
else:
    u = t + ' c3'
v_list = [u for _ in range(6)]
w_list = [v_list for _ in range(10)]
x_list = [w_list for _ in range(8)]
y = random.choice(x_list)
z = random.choice(y)
aa = random.choice(z)
ab = ''
for _ in range(7):
        if _ == 4:
            break
        ab += aa
ac = ''
for _ in range(6):
        if _ == 3:
            continue
        ac += ab
ad = f'string {ac}'
ae = ''
for _ in range(10):
        if _ == 5:
            break
        ae += ad
af = ae + '.'
ag_set = {af, af, af, af, af}
ag = random.choice(list(ag_set))
print(ag)