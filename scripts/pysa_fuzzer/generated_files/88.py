import random
import math
a = input()
b_list = [a for _ in range(4)]
c_list = [b_list for _ in range(10)]
d = random.choice(c_list)
e = random.choice(d)
f = (e, e, e)
g, h, i = f
j = g + h + i
def k():
    return j
def l():
    return k()
def m():
    return l()
n = m()
o = (n, n, n)
p, q, r = o
s = p + q + r
t_list = [s for _ in range(4)]
u_list = [t_list for _ in range(3)]
v_list = [u_list for _ in range(3)]
w = random.choice(v_list)
x = random.choice(w)
y = random.choice(x)
z = ''
for _ in range(5):
    for __ in range(4):
                z += y
aa_list = [z for _ in range(6)]
ab_list = [aa_list for _ in range(10)]
ac = random.choice(ab_list)
ad = random.choice(ac)
ae = ''
for _ in range(3):
    for __ in range(3):
                ae += ad
af_list = [ae for _ in range(3)]
ag = random.choice(af_list)
ah_list = [ag for _ in range(9)]
ai_list = [ah_list for _ in range(4)]
aj = random.choice(ai_list)
ak = random.choice(aj)
al = ''
for _ in range(5):
    for __ in range(2):
                al += ak
if al == '2':
    am = al + ' c1'
elif al == '11':
    am = al + ' c2'
else:
    am = al + ' c3'
an_dict = {38: am, 85: am, 9: am, 22: am, 17: am}
ao_dict = {7: an_dict, 92: an_dict}
ap = random.choice(list(ao_dict.values()))
aq = random.choice(list(ap.values()))
ar = aq + '3'
at = ar + '4'
au = ''
for _ in range(5):
        if _ == 1:
            continue
        au += at
av = ''
for _ in range(5):
    for __ in range(2):
                av += au
aw = [av for _ in range(8)]
random.shuffle(aw)
ax = random.choice(aw)
ay = ''
counteray = 0
while counteray < 5:
    ay += ax
    counteray += 1
print(ay)