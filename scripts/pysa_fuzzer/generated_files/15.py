import random
import math
a = input()
b = ''
for _ in range(4):
    b += a
c_list = [b for _ in range(8)]
d_list = [c_list for _ in range(9)]
e_list = [d_list for _ in range(4)]
f = random.choice(e_list)
g = random.choice(f)
h = random.choice(g)
i = h + '.'
j = i + '.'
k = j[0:]
def l():
    return k
def m():
    return l()
def n():
    return m()
o = n()
p_set = {o, o, o, o, o, o, o, o, o, o}
p = random.choice(list(p_set))
q_set = {p, p, p, p, p, p}
q = random.choice(list(q_set))
r = q + '3'
s = r + '1'
t = s + '7'
u = t + '.'
v_dict = {20: u, 29: u, 74: u, 79: u, 3: u, 92: u, 11: u, 56: u}
w = random.choice(list(v_dict.values()))
x = [w for _ in range(10)]
random.shuffle(x)
y = random.choice(x)
z = [y for _ in range(5)]
random.shuffle(z)
aa = random.choice(z)
ab = aa[0:]
ac = ab[0:]
ad = ''
counterad = 0
while counterad < 5:
    ae = ''
    counterae = 0
    while counterae < 5:
        af = ''
        counteraf = 0
        while counteraf < 2:
            af += ae
            counteraf += 1
            ae += ad
            counterae += 1
        ad += ac
        counterad += 1
ag = [af for _ in range(7)]
random.shuffle(ag)
ah = random.choice(ag)
ai = f'string {ah}'
print(ai)