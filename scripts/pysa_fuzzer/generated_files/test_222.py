import random
import math

a = input()
b_dict = {35: a, 97: a, 86: a, 33: a, 29: a}
c = random.choice(list(b_dict.values()))
d_set = {c, c, c, c, c, c, c}
d = random.choice(list(d_set))
e_list = [d for _ in range(9)]
f = random.choice(e_list)
g_list = [f for _ in range(6)]
h_list = [g_list for _ in range(6)]
i = random.choice(h_list)
j = random.choice(i)
k = j + '1'
l = k + '8'
m_set = {l, l, l, l, l, l, l}
m = random.choice(list(m_set))
n = (m, m, m)
o, p, q = n
r = o + p + q
s = r[0:]
t_list = [s for _ in range(5)]
u = random.choice(t_list)
v = ''
for _ in range(5):
    w = ''
    for _ in range(3):
        w += v
        v += u
x = w[0:]
y_list = [x for _ in range(5)]
z_list = [y_list for _ in range(3)]
aa_list = [z_list for _ in range(10)]
ab = random.choice(aa_list)
ac = random.choice(ab)
ad = random.choice(ac)
def ae():
    return ad
def af():
    return ae()
ag = af()
ah_set = {ag, ag, ag, ag, ag, ag, ag, ag}
ah = random.choice(list(ah_set))
def ai():
    return ah
aj = ai()
ak = aj[0:]
al = (ak, ak, ak)
am, an, ao = al
ap = am + an + ao
aq = ''
counteraq = 0
while counteraq < 3:
    ar = ''
    counterar = 0
    while counterar < 4:
        ar += aq
        counterar += 1
        aq += ap
        counteraq += 1
print(ar)