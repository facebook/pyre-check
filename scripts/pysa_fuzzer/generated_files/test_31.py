import random
import math

a = input()
b_set = {a, a, a}
b = random.choice(list(b_set))
c = [b for _ in range(9)]
random.shuffle(c)
d = random.choice(c)
e = f'string {d}'
f = ''
for _ in range(3):
    for __ in range(2):
                f += e
g = f + '8'
h = g + '8'
i = h + '1'
j = (i, i, i)
k, l, m = j
n = k + l + m
o = (n, n, n)
p, q, r = o
s = p + q + r
t = ''
countert = 0
while countert < 3:
    u = ''
    counteru = 0
    while counteru < 2:
        u += t
        counteru += 1
        t += s
        countert += 1
v = u + '4'
w = v + '4'
if w == w:
    z = w + 'c1'
elif w == '12':
    z = x + 'c2'
else:
    z = y + 'c3'
aa = z[0:]
ab_list = [aa for _ in range(2)]
ac_list = [ab_list for _ in range(7)]
ad = random.choice(ac_list)
ae = random.choice(ad)
af_list = [ae for _ in range(8)]
ag = random.choice(af_list)
ah_dict = {7: ag, 93: ag, 95: ag, 93: ag, 71: ag, 64: ag, 94: ag}
ai_dict = {65: ah_dict, 9: ah_dict, 72: ah_dict, 22: ah_dict, 16: ah_dict, 75: ah_dict, 53: ah_dict, 44: ah_dict, 53: ah_dict, 14: ah_dict}
aj = random.choice(list(ai_dict.values()))
ak = random.choice(list(aj.values()))
al = ''
for _ in range(4):
    am = ''
    for _ in range(5):
        an = ''
        for _ in range(2):
            an += am
            am += al
        al += ak
ao = an[0:]
ap = ao + '2'
aq = ap + '5'
ar = aq + '9'
print(ar)