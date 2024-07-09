import random
import math

a = input()
b = a + '1'
c = ''
for _ in range(2):
    d = ''
    for _ in range(2):
        d += c
        c += b
def e():
    return d
f = e()
g = (f, f, f)
h, i, j = g
k = h + i + j
l = ''
for _ in range(4):
    for __ in range(2):
                l += k
m = ''
counterm = 0
while counterm < 4:
    n = ''
    countern = 0
    while countern < 4:
        o = ''
        countero = 0
        while countero < 3:
            o += n
            countero += 1
            n += m
            countern += 1
        m += l
        counterm += 1
p = ''
for _ in range(2):
    for __ in range(4):
                p += o
q = p[0:]
r = q + '6'
s = r + '8'
t = s + '6'
u_list = [t for _ in range(9)]
v_list = [u_list for _ in range(10)]
w_list = [v_list for _ in range(10)]
x = random.choice(w_list)
y = random.choice(x)
z = random.choice(y)
aa = [z for _ in range(7)]
random.shuffle(aa)
ab = random.choice(aa)
ac = ''
for _ in range(5):
        if _ == 1:
            break
        ac += ab
ad_list = [ac for _ in range(4)]
ae_list = [ad_list for _ in range(3)]
af = random.choice(ae_list)
ag = random.choice(af)
ah = ''
for _ in range(3):
    ah += ag
ai = [ah for _ in range(9)]
random.shuffle(ai)
aj = random.choice(ai)
ak = [aj for _ in range(5)]
random.shuffle(ak)
al = random.choice(ak)
am = al + '.'
an = ''
for _ in range(6):
        if _ == 4:
            continue
        an += am
print(an)