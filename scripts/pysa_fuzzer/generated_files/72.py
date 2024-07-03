import random
import math
a = input()
b_set = {a, a, a, a, a}
b = random.choice(list(b_set))
c_dict = {36: b, 59: b, 64: b, 54: b, 37: b}
d_dict = {79: c_dict, 23: c_dict}
e_dict = {21: d_dict, 8: d_dict, 50: d_dict}
f = random.choice(list(e_dict.values()))
g = random.choice(list(f.values()))
h = random.choice(list(g.values()))
i = h[0:]
def j():
    return i
def k():
    return j()
l = k()
m_list = [l for _ in range(2)]
n_list = [m_list for _ in range(6)]
o_list = [n_list for _ in range(7)]
p = random.choice(o_list)
q = random.choice(p)
r = random.choice(q)
s = ''
for _ in range(2):
    for __ in range(2):
                s += r
t = s + '7'
u = t + '8'
v = u + '6'
w_set = {v, v, v, v, v, v, v}
w = random.choice(list(w_set))
x = [w for _ in range(7)]
random.shuffle(x)
y = random.choice(x)
z = ''
for _ in range(7):
        if _ == 3:
            break
        z += y
aa_dict = {5: z, 40: z, 26: z}
ab_dict = {93: aa_dict, 38: aa_dict, 18: aa_dict, 76: aa_dict, 40: aa_dict, 53: aa_dict, 22: aa_dict, 53: aa_dict, 64: aa_dict}
ac = random.choice(list(ab_dict.values()))
ad = random.choice(list(ac.values()))
ae = ''
for _ in range(9):
        if _ == 2:
            continue
        ae += ad
af_dict = {83: ae, 4: ae, 68: ae, 73: ae, 62: ae, 46: ae, 62: ae, 98: ae, 4: ae}
ag_dict = {42: af_dict, 95: af_dict, 7: af_dict, 42: af_dict, 39: af_dict, 49: af_dict, 9: af_dict, 82: af_dict, 33: af_dict, 47: af_dict}
ah_dict = {53: ag_dict, 18: ag_dict, 82: ag_dict}
ai = random.choice(list(ah_dict.values()))
aj = random.choice(list(ai.values()))
ak = random.choice(list(aj.values()))
if ak == '6':
    al = ak + ' c1'
elif ak == '13':
    al = ak + ' c2'
else:
    al = ak + ' c3'
am = ''
for _ in range(3):
    for __ in range(5):
                am += al
an = ''
for _ in range(2):
    for __ in range(2):
                an += am
ao = ''
for _ in range(10):
        if _ == 4:
            break
        ao += an
ap = ao[0:]
print(ap)