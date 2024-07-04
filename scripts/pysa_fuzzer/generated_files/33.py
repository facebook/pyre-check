import random
import math
a = input()
b = ''
counterb = 0
while counterb < 4:
    c = ''
    counterc = 0
    while counterc < 5:
        d = ''
        counterd = 0
        while counterd < 2:
            d += c
            counterd += 1
            c += b
            counterc += 1
        b += a
        counterb += 1
e_list = [d for _ in range(5)]
f_list = [e_list for _ in range(7)]
g = random.choice(f_list)
h = random.choice(g)
i = [h for _ in range(7)]
random.shuffle(i)
j = random.choice(i)
k_dict = {74: j, 3: j, 65: j, 53: j}
l_dict = {29: k_dict, 82: k_dict, 73: k_dict, 4: k_dict, 22: k_dict, 84: k_dict}
m = random.choice(list(l_dict.values()))
n = random.choice(list(m.values()))
o = [n for _ in range(9)]
random.shuffle(o)
p = random.choice(o)
q = p + '3'
def r():
    return q
def s():
    return r()
t = s()
def u():
    return t
v = u()
w = ''
for _ in range(5):
    w += v
x_list = [w for _ in range(4)]
y_list = [x_list for _ in range(5)]
z_list = [y_list for _ in range(10)]
aa = random.choice(z_list)
ab = random.choice(aa)
ac = random.choice(ab)
ad = ''
counterad = 0
while counterad < 5:
    ae = ''
    counterae = 0
    while counterae < 2:
        ae += ad
        counterae += 1
        ad += ac
        counterad += 1
def af():
    return ae
def ag():
    return af()
ah = ag()
def ai():
    return ah
def aj():
    return ai()
ak = aj()
al_set = {ak, ak, ak, ak, ak, ak}
al = random.choice(list(al_set))
am = [al for _ in range(5)]
random.shuffle(am)
an = random.choice(am)
ao = an + '8'
ap = ao + '2'
aq = [ap for _ in range(10)]
random.shuffle(aq)
ar = random.choice(aq)
print(ar)