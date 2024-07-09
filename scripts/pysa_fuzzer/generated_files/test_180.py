import random
import math

a = input()
b = ''
for _ in range(4):
    for __ in range(2):
                b += a
c_set = {b, b, b, b, b}
c = random.choice(list(c_set))
d = [c for _ in range(8)]
random.shuffle(d)
e = random.choice(d)
f = ''
for _ in range(10):
        if _ == 2:
            break
        f += e
g = ''
for _ in range(5):
        if _ == 4:
            break
        g += f
h = g + '.'
i = f'string {h}'
j_list = [i for _ in range(8)]
k_list = [j_list for _ in range(8)]
l = random.choice(k_list)
m = random.choice(l)
n = ''
for _ in range(5):
    o = ''
    for _ in range(2):
        o += n
        n += m
p = o + '7'
q = p + '6'
r = q + '4'
s = r + '2'
t_dict = {57: s, 76: s, 80: s, 54: s, 47: s, 16: s, 95: s}
u_dict = {97: t_dict, 96: t_dict, 4: t_dict, 44: t_dict, 41: t_dict, 75: t_dict, 47: t_dict, 27: t_dict, 31: t_dict}
v_dict = {19: u_dict, 23: u_dict, 24: u_dict, 17: u_dict, 43: u_dict, 52: u_dict}
w = random.choice(list(v_dict.values()))
x = random.choice(list(w.values()))
y = random.choice(list(x.values()))
z_list = [y for _ in range(3)]
aa_list = [z_list for _ in range(10)]
ab_list = [aa_list for _ in range(10)]
ac = random.choice(ab_list)
ad = random.choice(ac)
ae = random.choice(ad)
if ae == ae:
    ah = ae + 'c1'
elif ae == '14':
    ah = af + 'c2'
else:
    ah = ag + 'c3'
ai = ah + '9'
aj_set = {ai, ai, ai, ai, ai, ai, ai, ai}
aj = random.choice(list(aj_set))
ak = ''
for _ in range(7):
        if _ == 4:
            break
        ak += aj
al = f'string {ak}'
print(al)