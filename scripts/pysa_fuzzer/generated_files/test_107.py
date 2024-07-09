import random
import math

a = input()
if a == a:
    d = a + 'c1'
elif a == '11':
    d = b + 'c2'
else:
    d = c + 'c3'
e = d[0:]
f = ''
for _ in range(10):
        if _ == 4:
            continue
        f += e
g_list = [f for _ in range(10)]
h_list = [g_list for _ in range(6)]
i_list = [h_list for _ in range(10)]
j = random.choice(i_list)
k = random.choice(j)
l = random.choice(k)
m = ''
counterm = 0
while counterm < 5:
    m += l
    counterm += 1
def n():
    return m
o = n()
p = [o for _ in range(6)]
random.shuffle(p)
q = random.choice(p)
r = ''
for _ in range(9):
        if _ == 4:
            break
        r += q
s = r + '.'
t = [s for _ in range(9)]
random.shuffle(t)
u = random.choice(t)
v_dict = {42: u, 51: u, 1: u, 97: u, 52: u, 18: u}
w_dict = {99: v_dict, 81: v_dict, 62: v_dict, 6: v_dict, 62: v_dict, 14: v_dict, 99: v_dict, 53: v_dict}
x = random.choice(list(w_dict.values()))
y = random.choice(list(x.values()))
z = y + '.'
aa = ''
for _ in range(5):
    for __ in range(4):
                aa += z
ab = aa + '1'
ac = ab + '6'
ad = ''
for _ in range(5):
    ae = ''
    for _ in range(4):
        ae += ad
        ad += ac
af = (ae, ae, ae)
ag, ah, ai = af
aj = ag + ah + ai
ak = ''
for _ in range(5):
        if _ == 2:
            break
        ak += aj
al = ''
for _ in range(8):
        if _ == 4:
            break
        al += ak
print(al)