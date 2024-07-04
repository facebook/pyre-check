import random
import math
a = input()
b_dict = {93: a, 92: a, 42: a, 43: a, 2: a, 71: a, 40: a}
c_dict = {43: b_dict, 75: b_dict, 96: b_dict, 25: b_dict, 45: b_dict, 87: b_dict, 19: b_dict, 18: b_dict}
d = random.choice(list(c_dict.values()))
e = random.choice(list(d.values()))
f_dict = {18: e, 19: e, 26: e, 15: e}
g = random.choice(list(f_dict.values()))
h_list = [g for _ in range(6)]
i_list = [h_list for _ in range(2)]
j_list = [i_list for _ in range(9)]
k = random.choice(j_list)
l = random.choice(k)
m = random.choice(l)
n = m + '2'
o = n + '3'
p = o + '4'
q = ''
counterq = 0
while counterq < 3:
    r = ''
    counterr = 0
    while counterr < 3:
        s = ''
        counters = 0
        while counters < 5:
            s += r
            counters += 1
            r += q
            counterr += 1
        q += p
        counterq += 1
def t():
    return s
u = t()
v = u[0:]
w_set = {v, v}
w = random.choice(list(w_set))
x_dict = {62: w, 2: w, 45: w, 68: w, 62: w, 30: w, 73: w, 39: w, 23: w}
y_dict = {71: x_dict, 43: x_dict, 74: x_dict, 2: x_dict, 52: x_dict}
z = random.choice(list(y_dict.values()))
aa = random.choice(list(z.values()))
ab_list = [aa for _ in range(3)]
ac_list = [ab_list for _ in range(5)]
ad_list = [ac_list for _ in range(8)]
ae = random.choice(ad_list)
af = random.choice(ae)
ag = random.choice(af)
ah = f'string {ag}'
ai = (ah, ah, ah)
aj, ak, al = ai
am = aj + ak + al
if am == '2':
    an = am + ' c1'
elif am == '16':
    an = am + ' c2'
else:
    an = am + ' c3'
ao = an + '.'
ap = ''
counterap = 0
while counterap < 4:
    aq = ''
    counteraq = 0
    while counteraq < 2:
        ar = ''
        counterar = 0
        while counterar < 5:
            ar += aq
            counterar += 1
            aq += ap
            counteraq += 1
        ap += ao
        counterap += 1
at = ''
for _ in range(4):
    for __ in range(3):
                at += ar
au = at + '9'
av = au + '2'
aw = av + '7'
ax = aw + '4'
print(ax)