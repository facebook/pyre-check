import random
import math

a = input()
b = ''
for _ in range(9):
        if _ == 3:
            break
        b += a
c_list = [b for _ in range(2)]
d_list = [c_list for _ in range(8)]
e_list = [d_list for _ in range(2)]
f = random.choice(e_list)
g = random.choice(f)
h = random.choice(g)
i = [h for _ in range(7)]
random.shuffle(i)
j = random.choice(i)
k_set = {j, j, j, j, j, j}
k = random.choice(list(k_set))
l = [k for _ in range(8)]
random.shuffle(l)
m = random.choice(l)
n = ''
for _ in range(5):
        if _ == 3:
            break
        n += m
o = n + '.'
p = f'string {o}'
q = p + '.'
r = ''
for _ in range(6):
        if _ == 3:
            continue
        r += q
def s():
    return r
t = s()
u = t + '3'
v = u + '3'
w = v + '7'
def x():
    return w
def y():
    return x()
z = y()
aa = [z for _ in range(9)]
random.shuffle(aa)
ab = random.choice(aa)
ac_dict = {63: ab, 83: ab, 73: ab, 39: ab, 84: ab, 71: ab, 91: ab, 99: ab, 29: ab, 37: ab}
ad_dict = {61: ac_dict, 15: ac_dict, 3: ac_dict, 26: ac_dict, 29: ac_dict, 12: ac_dict, 66: ac_dict, 9: ac_dict, 59: ac_dict, 46: ac_dict}
ae = random.choice(list(ad_dict.values()))
af = random.choice(list(ae.values()))
ag_dict = {50: af, 58: af, 52: af, 52: af, 48: af, 100: af, 24: af, 56: af, 73: af, 37: af}
ah = random.choice(list(ag_dict.values()))
ai = ''
for _ in range(5):
    for __ in range(2):
                ai += ah
aj = ''
for _ in range(7):
        if _ == 4:
            continue
        aj += ai
print(aj)