import random
import math

a = input()
b = [a for _ in range(5)]
random.shuffle(b)
c = random.choice(b)
d_list = [c for _ in range(6)]
e = random.choice(d_list)
f_list = [e for _ in range(6)]
g_list = [f_list for _ in range(9)]
h = random.choice(g_list)
i = random.choice(h)
j = i[0:]
def k():
    return j
def l():
    return k()
def m():
    return l()
n = m()
o = [n for _ in range(6)]
random.shuffle(o)
p = random.choice(o)
q = f'string {p}'
r = q + '5'
s = r + '2'
t = s + '8'
u = ''
for _ in range(9):
        if _ == 3:
            break
        u += t
v = ''
for _ in range(7):
        if _ == 2:
            break
        v += u
w = ''
for _ in range(6):
        if _ == 2:
            continue
        w += v
x = w + '2'
y = x + '9'
z = y + '9'
aa = [z for _ in range(8)]
random.shuffle(aa)
ab = random.choice(aa)
ac_dict = {14: ab, 41: ab, 99: ab, 40: ab, 45: ab}
ad = random.choice(list(ac_dict.values()))
ae = [ad for _ in range(8)]
random.shuffle(ae)
af = random.choice(ae)
ag = ''
counterag = 0
while counterag < 3:
    ah = ''
    counterah = 0
    while counterah < 2:
        ah += ag
        counterah += 1
        ag += af
        counterag += 1
ai_list = [ah for _ in range(3)]
aj_list = [ai_list for _ in range(7)]
ak = random.choice(aj_list)
al = random.choice(ak)
am = al + '.'
print(am)