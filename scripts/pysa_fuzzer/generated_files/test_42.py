import random
import math

a = input()
b = [a for _ in range(6)]
random.shuffle(b)
c = random.choice(b)
d = (c, c, c)
e, f, g = d
h = e + f + g
i = [h for _ in range(9)]
random.shuffle(i)
j = random.choice(i)
k = ''
counterk = 0
while counterk < 4:
    l = ''
    counterl = 0
    while counterl < 5:
        l += k
        counterl += 1
        k += j
        counterk += 1
m_set = {l, l, l, l, l, l, l}
m = random.choice(list(m_set))
n = f'string {m}'
o = f'string {n}'
p = [o for _ in range(8)]
random.shuffle(p)
q = random.choice(p)
r_dict = {3: q, 21: q, 55: q}
s = random.choice(list(r_dict.values()))
def t():
    return s
u = t()
v = ''
for _ in range(3):
    for __ in range(4):
                v += u
w = f'string {v}'
x = ''
for _ in range(6):
        if _ == 1:
            break
        x += w
y_set = {x, x, x, x, x, x, x, x, x, x}
y = random.choice(list(y_set))
z = f'string {y}'
aa_list = [z for _ in range(10)]
ab_list = [aa_list for _ in range(3)]
ac_list = [ab_list for _ in range(5)]
ad = random.choice(ac_list)
ae = random.choice(ad)
af = random.choice(ae)
ag_list = [af for _ in range(8)]
ah_list = [ag_list for _ in range(6)]
ai_list = [ah_list for _ in range(10)]
aj = random.choice(ai_list)
ak = random.choice(aj)
al = random.choice(ak)
am = [al for _ in range(5)]
random.shuffle(am)
an = random.choice(am)
print(an)