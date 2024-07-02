import random
import math
a = input()
b_set = {a, a, a, a, a, a, a, a}
b = random.choice(list(b_set))
c_dict = {42: b, 29: b, 7: b, 6: b, 84: b, 93: b, 87: b}
d_dict = {30: c_dict, 28: c_dict, 83: c_dict, 63: c_dict, 62: c_dict, 47: c_dict, 76: c_dict}
e_dict = {2: d_dict, 6: d_dict, 73: d_dict, 10: d_dict, 90: d_dict, 97: d_dict, 9: d_dict, 25: d_dict, 13: d_dict, 15: d_dict}
f = random.choice(list(e_dict.values()))
g = random.choice(list(f.values()))
h = random.choice(list(g.values()))
i = f'string {h}'
def j():
    return i
def k():
    return j()
l = k()
m = ''
for _ in range(2):
    n = ''
    for _ in range(2):
        n += m
        m += l
o = ''
for _ in range(2):
    for __ in range(2):
                o += n
p = ''
for _ in range(6):
        if _ == 5:
            break
        p += o
q = ''
for _ in range(2):
    q += p
r = q + '6'
s = r + '9'
t_list = [s for _ in range(9)]
u = random.choice(t_list)
v_list = [u for _ in range(7)]
w_list = [v_list for _ in range(3)]
x = random.choice(w_list)
y = random.choice(x)
z = ''
for _ in range(4):
    for __ in range(3):
                z += y
aa = ''
for _ in range(8):
        if _ == 2:
            break
        aa += z
ab = aa[0:]
ac = (ab, ab, ab)
ad, ae, af = ac
ag = ad + ae + af
if ag == '1':
    ah = ag + ' c1'
elif ag == '14':
    ah = ag + ' c2'
else:
    ah = ag + ' c3'
ai = [ah for _ in range(6)]
random.shuffle(ai)
aj = random.choice(ai)
if aj == '8':
    ak = aj + ' c1'
elif aj == '19':
    ak = aj + ' c2'
else:
    ak = aj + ' c3'
print(ak)