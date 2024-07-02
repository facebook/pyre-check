import random
import math
a = input()
b = a + '8'
c = ''
for _ in range(3):
    d = ''
    for _ in range(3):
        d += c
        c += b
e = (d, d, d)
f, g, h = e
i = f + g + h
j = f'string {i}'
k = ''
counterk = 0
while counterk < 4:
    l = ''
    counterl = 0
    while counterl < 5:
        l += k
        counterl += 1
        k += j
        counterk += 1
m_list = [l for _ in range(9)]
n_list = [m_list for _ in range(6)]
o = random.choice(n_list)
p = random.choice(o)
def q():
    return p
r = q()
s = r + '.'
t = ''
for _ in range(5):
    t += s
u = t[0:]
v_dict = {25: u, 28: u, 42: u, 42: u, 20: u, 84: u, 60: u}
w_dict = {92: v_dict, 48: v_dict, 52: v_dict, 85: v_dict, 1: v_dict}
x_dict = {68: w_dict, 16: w_dict, 93: w_dict, 26: w_dict, 15: w_dict, 80: w_dict, 34: w_dict, 28: w_dict, 60: w_dict}
y = random.choice(list(x_dict.values()))
z = random.choice(list(y.values()))
aa = random.choice(list(z.values()))
ab = [aa for _ in range(7)]
random.shuffle(ab)
ac = random.choice(ab)
if ac == '7':
    ad = ac + ' c1'
elif ac == '11':
    ad = ac + ' c2'
else:
    ad = ac + ' c3'
ae = (ad, ad, ad)
af, ag, ah = ae
ai = af + ag + ah
def aj():
    return ai
def ak():
    return aj()
def al():
    return ak()
am = al()
an = [am for _ in range(10)]
random.shuffle(an)
ao = random.choice(an)
ap = ''
for _ in range(4):
    aq = ''
    for _ in range(4):
        ar = ''
        for _ in range(3):
            ar += aq
            aq += ap
        ap += ao
at_set = {ar, ar, ar, ar, ar, ar, ar, ar, ar, ar}
at = random.choice(list(at_set))
print(at)