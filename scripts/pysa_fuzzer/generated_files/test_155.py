import random
import math

a = input()
b_set = {a, a, a, a, a, a, a, a}
b = random.choice(list(b_set))
c = (b, b, b)
d, e, f = c
g = d + e + f
def h():
    return g
def i():
    return h()
def j():
    return i()
k = j()
l = k + '4'
m = ''
for _ in range(7):
        if _ == 3:
            break
        m += l
n = [m for _ in range(9)]
random.shuffle(n)
o = random.choice(n)
p = o + '.'
q = p + '.'
r = f'string {q}'
s_set = {r, r, r}
s = random.choice(list(s_set))
if s == s:
    v = s + 'c1'
elif s == '20':
    v = t + 'c2'
else:
    v = u + 'c3'
w_list = [v for _ in range(5)]
x_list = [w_list for _ in range(6)]
y_list = [x_list for _ in range(10)]
z = random.choice(y_list)
aa = random.choice(z)
ab = random.choice(aa)
ac_list = [ab for _ in range(9)]
ad = random.choice(ac_list)
ae = ''
counterae = 0
while counterae < 5:
    af = ''
    counteraf = 0
    while counteraf < 3:
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
elif ah == '18':
    ak = ai + 'c2'
else:
    ak = aj + 'c3'
al = ''
for _ in range(4):
    am = ''
    for _ in range(3):
        am += al
        al += ak
an = ''
for _ in range(8):
        if _ == 4:
            break
        an += am
print(an)