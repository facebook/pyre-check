import random
import math
a = input()
b = ''
for _ in range(4):
    b += a
c_list = [b for _ in range(9)]
d_list = [c_list for _ in range(7)]
e = random.choice(d_list)
f = random.choice(e)
g_dict = {13: f, 14: f, 37: f, 74: f}
h_dict = {81: g_dict, 60: g_dict, 29: g_dict, 74: g_dict, 40: g_dict, 66: g_dict, 39: g_dict, 99: g_dict, 40: g_dict}
i_dict = {46: h_dict, 60: h_dict, 59: h_dict, 11: h_dict, 7: h_dict, 2: h_dict}
j = random.choice(list(i_dict.values()))
k = random.choice(list(j.values()))
l = random.choice(list(k.values()))
m = (l, l, l)
n, o, p = m
q = n + o + p
r = (q, q, q)
s, t, u = r
v = s + t + u
w = ''
counterw = 0
while counterw < 2:
    w += v
    counterw += 1
x = w[0:]
y_list = [x for _ in range(4)]
z = random.choice(y_list)
aa = z[0:]
ab = ''
for _ in range(5):
    ac = ''
    for _ in range(5):
        ac += ab
        ab += aa
ad = ac + '.'
ae = [ad for _ in range(10)]
random.shuffle(ae)
af = random.choice(ae)
ag_dict = {67: af, 64: af, 24: af, 84: af, 48: af, 26: af, 73: af, 20: af, 99: af}
ah_dict = {53: ag_dict, 65: ag_dict, 3: ag_dict, 64: ag_dict, 15: ag_dict, 80: ag_dict, 55: ag_dict}
ai_dict = {6: ah_dict, 77: ah_dict, 88: ah_dict, 33: ah_dict, 80: ah_dict, 90: ah_dict, 43: ah_dict, 74: ah_dict}
aj = random.choice(list(ai_dict.values()))
ak = random.choice(list(aj.values()))
al = random.choice(list(ak.values()))
am = ''
counteram = 0
while counteram < 2:
    an = ''
    counteran = 0
    while counteran < 5:
        ao = ''
        counterao = 0
        while counterao < 5:
            ao += an
            counterao += 1
            an += am
            counteran += 1
        am += al
        counteram += 1
ap = ao + '.'
aq = ''
counteraq = 0
while counteraq < 2:
    ar = ''
    counterar = 0
    while counterar < 3:
        ar += aq
        counterar += 1
        aq += ap
        counteraq += 1
at_dict = {30: ar, 45: ar, 26: ar, 59: ar}
au_dict = {88: at_dict, 37: at_dict, 22: at_dict}
av_dict = {24: au_dict, 97: au_dict, 70: au_dict, 45: au_dict, 8: au_dict, 36: au_dict, 19: au_dict, 40: au_dict}
aw = random.choice(list(av_dict.values()))
ax = random.choice(list(aw.values()))
ay = random.choice(list(ax.values()))
az = ay + '3'
ba = az + '4'
bb = ba + '2'
print(bb)