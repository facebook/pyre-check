import random
import math

a = input()
def b():
    return a
def c():
    return b()
d = c()
e = ''
countere = 0
while countere < 3:
    e += d
    countere += 1
f = e + '.'
g = [f for _ in range(8)]
random.shuffle(g)
h = random.choice(g)
i = (h, h, h)
j, k, l = i
m = j + k + l
n = m + '4'
o = n + '6'
p = o + '9'
q_list = [p for _ in range(7)]
r_list = [q_list for _ in range(4)]
s_list = [r_list for _ in range(4)]
t = random.choice(s_list)
u = random.choice(t)
v = random.choice(u)
w = ''
for _ in range(5):
        if _ == 4:
            continue
        w += v
x_dict = {2: w, 52: w, 67: w, 66: w, 36: w}
y = random.choice(list(x_dict.values()))
z_list = [y for _ in range(9)]
aa_list = [z_list for _ in range(9)]
ab = random.choice(aa_list)
ac = random.choice(ab)
ad_list = [ac for _ in range(10)]
ae = random.choice(ad_list)
af = ae + '8'
ag = af + '4'
ah = ag + '2'
ai = ''
for _ in range(3):
    aj = ''
    for _ in range(5):
        ak = ''
        for _ in range(5):
            ak += aj
            aj += ai
        ai += ah
al = ak[0:]
am = f'string {al}'
an = am + '.'
ao = ''
for _ in range(5):
    ap = ''
    for _ in range(5):
        ap += ao
        ao += an
aq = ap + '.'
print(aq)