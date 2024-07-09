import random
import math

a = input()
b = a[0:]
c = b + '.'
d = [c for _ in range(9)]
random.shuffle(d)
e = random.choice(d)
f_list = [e for _ in range(9)]
g = random.choice(f_list)
h = ''
for _ in range(2):
    for __ in range(2):
                h += g
i_set = {h, h, h, h, h, h, h, h, h, h}
i = random.choice(list(i_set))
j = ''
counterj = 0
while counterj < 2:
    k = ''
    counterk = 0
    while counterk < 4:
        k += j
        counterk += 1
        j += i
        counterj += 1
l = (k, k, k)
m, n, o = l
p = m + n + o
q = ''
counterq = 0
while counterq < 2:
    r = ''
    counterr = 0
    while counterr < 5:
        s = ''
        counters = 0
        while counters < 2:
            s += r
            counters += 1
            r += q
            counterr += 1
        q += p
        counterq += 1
t_set = {s, s, s, s, s, s, s, s, s, s}
t = random.choice(list(t_set))
u = t[0:]
v_dict = {83: u, 70: u, 66: u, 16: u, 50: u, 20: u, 90: u, 46: u, 10: u, 82: u}
w_dict = {21: v_dict, 77: v_dict, 21: v_dict, 14: v_dict, 42: v_dict, 49: v_dict, 26: v_dict}
x = random.choice(list(w_dict.values()))
y = random.choice(list(x.values()))
z = y + '5'
if z == z:
    ac = z + 'c1'
elif z == '17':
    ac = aa + 'c2'
else:
    ac = ab + 'c3'
ad = ac[0:]
ae = ''
counterae = 0
while counterae < 3:
    af = ''
    counteraf = 0
    while counteraf < 5:
        af += ae
        counteraf += 1
        ae += ad
        counterae += 1
ag_set = {af, af, af, af, af, af, af, af}
ag = random.choice(list(ag_set))
ah = ag + '.'
print(ah)