import random
import math

a = input()
b = ''
for _ in range(4):
    c = ''
    for _ in range(5):
        d = ''
        for _ in range(4):
            d += c
            c += b
        b += a
e_list = [d for _ in range(3)]
f_list = [e_list for _ in range(10)]
g = random.choice(f_list)
h = random.choice(g)
i = ''
for _ in range(5):
    for __ in range(4):
                i += h
j = (i, i, i)
k, l, m = j
n = k + l + m
def o():
    return n
def p():
    return o()
def q():
    return p()
r = q()
s = r[0:]
t = [s for _ in range(7)]
random.shuffle(t)
u = random.choice(t)
v = u + '.'
w_list = [v for _ in range(9)]
x = random.choice(w_list)
y_list = [x for _ in range(4)]
z_list = [y_list for _ in range(6)]
aa_list = [z_list for _ in range(9)]
ab = random.choice(aa_list)
ac = random.choice(ab)
ad = random.choice(ac)
ae = ''
counterae = 0
while counterae < 4:
    af = ''
    counteraf = 0
    while counteraf < 3:
        ag = ''
        counterag = 0
        while counterag < 4:
            ag += af
            counterag += 1
            af += ae
            counteraf += 1
        ae += ad
        counterae += 1
ah_set = {ag, ag, ag}
ah = random.choice(list(ah_set))
ai_list = [ah for _ in range(6)]
aj = random.choice(ai_list)
def ak():
    return aj
def al():
    return ak()
def am():
    return al()
an = am()
ao_set = {an, an}
ao = random.choice(list(ao_set))
ap_set = {ao, ao, ao, ao}
ap = random.choice(list(ap_set))
aq = ''
for _ in range(10):
        if _ == 5:
            break
        aq += ap
if aq == aq:
    au = aq + 'c1'
elif aq == '13':
    au = ar + 'c2'
else:
    au = at + 'c3'
print(au)