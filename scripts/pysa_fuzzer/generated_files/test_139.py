import random
import math

a = input()
if a == a:
    d = a + 'c1'
elif a == '16':
    d = b + 'c2'
else:
    d = c + 'c3'
e = ''
for _ in range(6):
        if _ == 1:
            continue
        e += d
f = ''
for _ in range(5):
    g = ''
    for _ in range(4):
        g += f
        f += e
h_list = [g for _ in range(10)]
i_list = [h_list for _ in range(6)]
j_list = [i_list for _ in range(2)]
k = random.choice(j_list)
l = random.choice(k)
m = random.choice(l)
n = f'string {m}'
o = [n for _ in range(6)]
random.shuffle(o)
p = random.choice(o)
q = ''
for _ in range(5):
    q += p
r_set = {q, q, q, q, q}
r = random.choice(list(r_set))
s_list = [r for _ in range(7)]
t_list = [s_list for _ in range(6)]
u_list = [t_list for _ in range(8)]
v = random.choice(u_list)
w = random.choice(v)
x = random.choice(w)
y = f'string {x}'
z_list = [y for _ in range(7)]
aa_list = [z_list for _ in range(2)]
ab = random.choice(aa_list)
ac = random.choice(ab)
ad = f'string {ac}'
ae_dict = {54: ad, 95: ad, 34: ad}
af_dict = {81: ae_dict, 92: ae_dict, 38: ae_dict, 71: ae_dict, 47: ae_dict, 58: ae_dict, 92: ae_dict, 9: ae_dict, 64: ae_dict, 78: ae_dict}
ag = random.choice(list(af_dict.values()))
ah = random.choice(list(ag.values()))
ai_set = {ah, ah, ah, ah, ah, ah, ah, ah}
ai = random.choice(list(ai_set))
aj_set = {ai, ai, ai}
aj = random.choice(list(aj_set))
ak = f'string {aj}'
al = ''
for _ in range(4):
    for __ in range(3):
                al += ak
am = al + '4'
an = am + '9'
print(an)