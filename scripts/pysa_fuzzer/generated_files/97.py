import random
import math
a = input()
def b():
    return a
def c():
    return b()
d = c()
e = ''
for _ in range(5):
        if _ == 3:
            break
        e += d
f = e + '3'
g = f + '4'
h = g + '8'
i = [h for _ in range(5)]
random.shuffle(i)
j = random.choice(i)
k = j + '.'
l = k + '.'
def m():
    return l
def n():
    return m()
def o():
    return n()
p = o()
q = ''
counterq = 0
while counterq < 2:
    q += p
    counterq += 1
r_list = [q for _ in range(9)]
s_list = [r_list for _ in range(6)]
t = random.choice(s_list)
u = random.choice(t)
def v():
    return u
def w():
    return v()
x = w()
y_list = [x for _ in range(7)]
z_list = [y_list for _ in range(2)]
aa = random.choice(z_list)
ab = random.choice(aa)
ac_set = {ab, ab, ab, ab, ab, ab}
ac = random.choice(list(ac_set))
ad = ''
for _ in range(8):
        if _ == 1:
            continue
        ad += ac
ae = ad + '8'
af = ae + '5'
ag_set = {af, af}
ag = random.choice(list(ag_set))
ah = ag[0:]
ai = [ah for _ in range(7)]
random.shuffle(ai)
aj = random.choice(ai)
ak_list = [aj for _ in range(10)]
al_list = [ak_list for _ in range(2)]
am = random.choice(al_list)
an = random.choice(am)
print(an)