import random
import math
a = input()
b_list = [a for _ in range(6)]
c_list = [b_list for _ in range(7)]
d_list = [c_list for _ in range(6)]
e = random.choice(d_list)
f = random.choice(e)
g = random.choice(f)
h = g + '8'
i = h + '2'
j = i + '4'
k = ''
for _ in range(2):
    for __ in range(4):
                k += j
l = k + '1'
m = l + '3'
n = ''
for _ in range(5):
    n += m
def o():
    return n
def p():
    return o()
def q():
    return p()
r = q()
def s():
    return r
t = s()
u = ''
for _ in range(5):
    for __ in range(4):
                u += t
v = ''
counterv = 0
while counterv < 2:
    v += u
    counterv += 1
w = v[0:]
x = ''
counterx = 0
while counterx < 2:
    y = ''
    countery = 0
    while countery < 5:
        y += x
        countery += 1
        x += w
        counterx += 1
z_list = [y for _ in range(3)]
aa_list = [z_list for _ in range(2)]
ab = random.choice(aa_list)
ac = random.choice(ab)
ad = ''
for _ in range(2):
    ae = ''
    for _ in range(5):
        af = ''
        for _ in range(2):
            af += ae
            ae += ad
        ad += ac
ag_dict = {51: af, 68: af, 87: af}
ah_dict = {87: ag_dict, 5: ag_dict}
ai_dict = {20: ah_dict, 96: ah_dict, 87: ah_dict}
aj = random.choice(list(ai_dict.values()))
ak = random.choice(list(aj.values()))
al = random.choice(list(ak.values()))
am = al[0:]
an = am + '.'
ao_set = {an, an, an}
ao = random.choice(list(ao_set))
ap = ao + '8'
aq = ap + '5'
ar = aq + '1'
print(ar)