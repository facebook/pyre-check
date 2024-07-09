import random
import math

a = input()
b_list = [a for _ in range(9)]
c_list = [b_list for _ in range(10)]
d_list = [c_list for _ in range(6)]
e = random.choice(d_list)
f = random.choice(e)
g = random.choice(f)
h_list = [g for _ in range(8)]
i_list = [h_list for _ in range(7)]
j = random.choice(i_list)
k = random.choice(j)
l_list = [k for _ in range(9)]
m_list = [l_list for _ in range(6)]
n = random.choice(m_list)
o = random.choice(n)
p_dict = {99: o, 9: o, 80: o, 36: o, 31: o, 44: o, 56: o, 92: o, 51: o}
q = random.choice(list(p_dict.values()))
r = q[0:]
s = ''
for _ in range(10):
        if _ == 3:
            break
        s += r
t_list = [s for _ in range(3)]
u = random.choice(t_list)
if u == u:
    x = u + 'c1'
elif u == '20':
    x = v + 'c2'
else:
    x = w + 'c3'
y = x + '.'
z = f'string {y}'
aa = f'string {z}'
ab = (aa, aa, aa)
ac, ad, ae = ab
af = ac + ad + ae
ag = af + '.'
ah = ''
for _ in range(4):
    for __ in range(4):
                ah += ag
ai = ''
for _ in range(2):
    for __ in range(5):
                ai += ah
aj = [ai for _ in range(5)]
random.shuffle(aj)
ak = random.choice(aj)
al_set = {ak, ak, ak, ak, ak, ak, ak, ak, ak}
al = random.choice(list(al_set))
am = ''
for _ in range(4):
    for __ in range(5):
                am += al
print(am)