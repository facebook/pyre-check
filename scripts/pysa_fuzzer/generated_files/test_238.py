import random
import math

a = input()
b_set = {a, a, a, a, a, a, a}
b = random.choice(list(b_set))
c_dict = {98: b, 32: b, 59: b}
d_dict = {66: c_dict, 69: c_dict, 16: c_dict, 82: c_dict, 78: c_dict, 90: c_dict, 78: c_dict, 17: c_dict}
e_dict = {48: d_dict, 82: d_dict, 77: d_dict}
f = random.choice(list(e_dict.values()))
g = random.choice(list(f.values()))
h = random.choice(list(g.values()))
i = ''
for _ in range(8):
        if _ == 1:
            continue
        i += h
j = i + '8'
k = j + '3'
l = k + '8'
m = f'string {l}'
n = [m for _ in range(10)]
random.shuffle(n)
o = random.choice(n)
p_list = [o for _ in range(10)]
q = random.choice(p_list)
r = (q, q, q)
s, t, u = r
v = s + t + u
w_set = {v, v, v, v, v}
w = random.choice(list(w_set))
x_set = {w, w, w, w, w, w, w, w, w, w}
x = random.choice(list(x_set))
def y():
    return x
def z():
    return y()
aa = z()
ab = ''
for _ in range(8):
        if _ == 3:
            continue
        ab += aa
ac_dict = {34: ab, 72: ab, 76: ab, 43: ab, 50: ab, 98: ab, 4: ab}
ad_dict = {10: ac_dict, 91: ac_dict, 27: ac_dict, 74: ac_dict}
ae = random.choice(list(ad_dict.values()))
af = random.choice(list(ae.values()))
def ag():
    return af
def ah():
    return ag()
def ai():
    return ah()
aj = ai()
if aj == aj:
    am = aj + 'c1'
elif aj == '16':
    am = ak + 'c2'
else:
    am = al + 'c3'
def an():
    return am
def ao():
    return an()
def ap():
    return ao()
aq = ap()
ar_list = [aq for _ in range(6)]
at_list = [ar_list for _ in range(9)]
au = random.choice(at_list)
av = random.choice(au)
aw = ''
for _ in range(9):
        if _ == 4:
            break
        aw += av
print(aw)