import random
import math

a = input()
b_set = {a, a, a}
b = random.choice(list(b_set))
c_list = [b for _ in range(2)]
d_list = [c_list for _ in range(5)]
e_list = [d_list for _ in range(2)]
f = random.choice(e_list)
g = random.choice(f)
h = random.choice(g)
i_list = [h for _ in range(5)]
j = random.choice(i_list)
k_set = {j, j, j, j, j, j}
k = random.choice(list(k_set))
l = ''
for _ in range(7):
        if _ == 5:
            continue
        l += k
m = ''
for _ in range(3):
    n = ''
    for _ in range(3):
        o = ''
        for _ in range(2):
            o += n
            n += m
        m += l
def p():
    return o
def q():
    return p()
r = q()
s = ''
for _ in range(2):
    t = ''
    for _ in range(4):
        u = ''
        for _ in range(2):
            u += t
            t += s
        s += r
v_dict = {7: u, 53: u, 71: u, 11: u, 13: u, 83: u, 40: u, 57: u}
w_dict = {47: v_dict, 41: v_dict, 9: v_dict, 15: v_dict, 63: v_dict, 71: v_dict}
x_dict = {24: w_dict, 94: w_dict, 51: w_dict, 18: w_dict, 19: w_dict, 2: w_dict, 40: w_dict}
y = random.choice(list(x_dict.values()))
z = random.choice(list(y.values()))
aa = random.choice(list(z.values()))
ab = ''
for _ in range(6):
        if _ == 5:
            continue
        ab += aa
ac = ab + '.'
ad_dict = {45: ac, 42: ac, 46: ac, 79: ac, 67: ac, 13: ac, 65: ac}
ae = random.choice(list(ad_dict.values()))
af = ae + '1'
ag = af + '3'
ah = f'string {ag}'
ai = ah[0:]
if ai == ai:
    al = ai + 'c1'
elif ai == '19':
    al = aj + 'c2'
else:
    al = ak + 'c3'
am = ''
counteram = 0
while counteram < 4:
    am += al
    counteram += 1
an = (am, am, am)
ao, ap, aq = an
ar = ao + ap + aq
print(ar)