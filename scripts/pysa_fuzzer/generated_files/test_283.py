import random
import math

a = input()
b = ''
for _ in range(4):
    b += a
c = (b, b, b)
d, e, f = c
g = d + e + f
h = ''
for _ in range(6):
        if _ == 2:
            break
        h += g
i = [h for _ in range(6)]
random.shuffle(i)
j = random.choice(i)
def k():
    return j
def l():
    return k()
def m():
    return l()
n = m()
o = [n for _ in range(6)]
random.shuffle(o)
p = random.choice(o)
q_list = [p for _ in range(7)]
r_list = [q_list for _ in range(5)]
s = random.choice(r_list)
t = random.choice(s)
u = ''
counteru = 0
while counteru < 3:
    v = ''
    counterv = 0
    while counterv < 3:
        w = ''
        counterw = 0
        while counterw < 5:
            w += v
            counterw += 1
            v += u
            counterv += 1
        u += t
        counteru += 1
if w == w:
    z = w + 'c1'
elif w == '19':
    z = x + 'c2'
else:
    z = y + 'c3'
aa = ''
for _ in range(6):
        if _ == 4:
            continue
        aa += z
ab = aa + '.'
ac_list = [ab for _ in range(2)]
ad_list = [ac_list for _ in range(10)]
ae_list = [ad_list for _ in range(4)]
af = random.choice(ae_list)
ag = random.choice(af)
ah = random.choice(ag)
ai_set = {ah, ah, ah, ah, ah, ah, ah}
ai = random.choice(list(ai_set))
aj = [ai for _ in range(8)]
random.shuffle(aj)
ak = random.choice(aj)
al_dict = {59: ak, 62: ak, 3: ak, 7: ak, 29: ak, 51: ak}
am_dict = {28: al_dict, 42: al_dict, 61: al_dict, 93: al_dict, 75: al_dict}
an = random.choice(list(am_dict.values()))
ao = random.choice(list(an.values()))
ap = ao + '.'
aq_list = [ap for _ in range(6)]
ar = random.choice(aq_list)
at_dict = {82: ar, 87: ar, 83: ar, 35: ar, 12: ar, 30: ar}
au_dict = {66: at_dict, 77: at_dict, 12: at_dict}
av_dict = {96: au_dict, 82: au_dict, 5: au_dict, 41: au_dict, 90: au_dict, 10: au_dict, 25: au_dict}
aw = random.choice(list(av_dict.values()))
ax = random.choice(list(aw.values()))
ay = random.choice(list(ax.values()))
print(ay)