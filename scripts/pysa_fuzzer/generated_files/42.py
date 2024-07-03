import random
import math
a = input()
b = a + '6'
c = b + '6'
d = c + '8'
e_list = [d for _ in range(3)]
f_list = [e_list for _ in range(7)]
g_list = [f_list for _ in range(9)]
h = random.choice(g_list)
i = random.choice(h)
j = random.choice(i)
k = j + '.'
l_list = [k for _ in range(5)]
m_list = [l_list for _ in range(5)]
n_list = [m_list for _ in range(9)]
o = random.choice(n_list)
p = random.choice(o)
q = random.choice(p)
def r():
    return q
def s():
    return r()
def t():
    return s()
u = t()
v = [u for _ in range(5)]
random.shuffle(v)
w = random.choice(v)
x_list = [w for _ in range(4)]
y = random.choice(x_list)
z = ''
for _ in range(4):
    z += y
aa = ''
for _ in range(6):
        if _ == 2:
            continue
        aa += z
ab = ''
counterab = 0
while counterab < 2:
    ac = ''
    counterac = 0
    while counterac < 5:
        ad = ''
        counterad = 0
        while counterad < 3:
            ad += ac
            counterad += 1
            ac += ab
            counterac += 1
        ab += aa
        counterab += 1
ae = ''
for _ in range(5):
    ae += ad
if ae == '3':
    af = ae + ' c1'
elif ae == '18':
    af = ae + ' c2'
else:
    af = ae + ' c3'
ag = af[0:]
ah = ''
counterah = 0
while counterah < 2:
    ah += ag
    counterah += 1
def ai():
    return ah
def aj():
    return ai()
ak = aj()
al = f'string {ak}'
am = ''
counteram = 0
while counteram < 3:
    an = ''
    counteran = 0
    while counteran < 2:
        ao = ''
        counterao = 0
        while counterao < 4:
            ao += an
            counterao += 1
            an += am
            counteran += 1
        am += al
        counteram += 1
ap = ao + '.'
print(ap)