import random
import math
a = input()
b_set = {a, a, a, a}
b = random.choice(list(b_set))
c_list = [b for _ in range(6)]
d = random.choice(c_list)
e = ''
for _ in range(7):
        if _ == 4:
            continue
        e += d
f_set = {e, e, e, e}
f = random.choice(list(f_set))
g = ''
for _ in range(7):
        if _ == 3:
            continue
        g += f
h = g + '.'
i = f'string {h}'
j = ''
for _ in range(2):
    k = ''
    for _ in range(3):
        l = ''
        for _ in range(3):
            l += k
            k += j
        j += i
m_dict = {75: l, 90: l, 16: l, 17: l, 67: l}
n_dict = {64: m_dict, 62: m_dict, 29: m_dict, 83: m_dict, 17: m_dict, 9: m_dict, 84: m_dict, 97: m_dict}
o = random.choice(list(n_dict.values()))
p = random.choice(list(o.values()))
q = ''
counterq = 0
while counterq < 2:
    r = ''
    counterr = 0
    while counterr < 3:
        r += q
        counterr += 1
        q += p
        counterq += 1
s_list = [r for _ in range(7)]
t = random.choice(s_list)
u_dict = {54: t, 17: t, 45: t, 60: t, 26: t, 70: t, 52: t, 13: t, 59: t}
v_dict = {58: u_dict, 72: u_dict, 49: u_dict, 94: u_dict, 23: u_dict, 43: u_dict, 76: u_dict, 2: u_dict}
w_dict = {54: v_dict, 85: v_dict, 28: v_dict, 84: v_dict, 67: v_dict, 88: v_dict}
x = random.choice(list(w_dict.values()))
y = random.choice(list(x.values()))
z = random.choice(list(y.values()))
aa = z + '7'
ab = aa + '5'
ac = ab + '6'
ad = (ac, ac, ac)
ae, af, ag = ad
ah = ae + af + ag
ai_list = [ah for _ in range(5)]
aj = random.choice(ai_list)
if aj == '4':
    ak = aj + ' c1'
elif aj == '20':
    ak = aj + ' c2'
else:
    ak = aj + ' c3'
al = ''
for _ in range(3):
    am = ''
    for _ in range(3):
        am += al
        al += ak
an = [am for _ in range(8)]
random.shuffle(an)
ao = random.choice(an)
print(ao)