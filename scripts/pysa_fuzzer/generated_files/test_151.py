import random
import math

a = input()
b = ''
counterb = 0
while counterb < 2:
    c = ''
    counterc = 0
    while counterc < 5:
        c += b
        counterc += 1
        b += a
        counterb += 1
d = ''
for _ in range(9):
        if _ == 1:
            continue
        d += c
e = [d for _ in range(9)]
random.shuffle(e)
f = random.choice(e)
g = ''
counterg = 0
while counterg < 2:
    g += f
    counterg += 1
h = f'string {g}'
def i():
    return h
def j():
    return i()
def k():
    return j()
l = k()
m = (l, l, l)
n, o, p = m
q = n + o + p
r = ''
for _ in range(5):
    for __ in range(4):
                r += q
s_set = {r, r, r, r, r, r}
s = random.choice(list(s_set))
t_list = [s for _ in range(5)]
u = random.choice(t_list)
v_dict = {93: u, 17: u, 78: u, 41: u, 82: u, 76: u, 44: u}
w_dict = {21: v_dict, 57: v_dict, 91: v_dict, 1: v_dict, 73: v_dict, 2: v_dict, 74: v_dict, 47: v_dict, 12: v_dict, 63: v_dict}
x_dict = {42: w_dict, 66: w_dict, 44: w_dict, 5: w_dict, 44: w_dict, 99: w_dict, 77: w_dict, 96: w_dict}
y = random.choice(list(x_dict.values()))
z = random.choice(list(y.values()))
aa = random.choice(list(z.values()))
ab_list = [aa for _ in range(6)]
ac = random.choice(ab_list)
ad = ac[0:]
ae = ''
counterae = 0
while counterae < 3:
    af = ''
    counteraf = 0
    while counteraf < 4:
        ag = ''
        counterag = 0
        while counterag < 3:
            ag += af
            counterag += 1
            af += ae
            counteraf += 1
        ae += ad
        counterae += 1
ah = ag[0:]
if ah == ah:
    ak = ah + 'c1'
elif ah == '19':
    ak = ai + 'c2'
else:
    ak = aj + 'c3'
al = (ak, ak, ak)
am, an, ao = al
ap = am + an + ao
aq_set = {ap, ap, ap, ap, ap, ap, ap, ap}
aq = random.choice(list(aq_set))
print(aq)