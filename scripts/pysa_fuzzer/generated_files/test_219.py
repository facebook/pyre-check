import random
import math

a = input()
b = ''
for _ in range(3):
    for __ in range(3):
                b += a
c = b[0:]
d = ''
for _ in range(2):
    for __ in range(3):
                d += c
e_set = {d, d, d, d, d, d, d, d}
e = random.choice(list(e_set))
f_list = [e for _ in range(5)]
g_list = [f_list for _ in range(9)]
h_list = [g_list for _ in range(2)]
i = random.choice(h_list)
j = random.choice(i)
k = random.choice(j)
l = k + '9'
m = l + '6'
n = ''
for _ in range(10):
        if _ == 4:
            break
        n += m
def o():
    return n
def p():
    return o()
q = p()
r = ''
for _ in range(10):
        if _ == 5:
            continue
        r += q
s_dict = {30: r, 83: r, 43: r, 15: r, 50: r, 37: r, 74: r, 99: r, 10: r}
t_dict = {70: s_dict, 16: s_dict, 41: s_dict, 12: s_dict, 61: s_dict, 14: s_dict, 66: s_dict}
u = random.choice(list(t_dict.values()))
v = random.choice(list(u.values()))
w_list = [v for _ in range(2)]
x = random.choice(w_list)
y = [x for _ in range(5)]
random.shuffle(y)
z = random.choice(y)
aa = ''
for _ in range(3):
    ab = ''
    for _ in range(2):
        ac = ''
        for _ in range(4):
            ac += ab
            ab += aa
        aa += z
def ad():
    return ac
ae = ad()
af = ae + '7'
ag = af + '2'
ah = ag + '8'
ai = ''
for _ in range(6):
        if _ == 5:
            break
        ai += ah
aj = ai + '.'
ak = aj[0:]
print(ak)