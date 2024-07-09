import random
import math

a = input()
b = ''
counterb = 0
while counterb < 5:
    c = ''
    counterc = 0
    while counterc < 5:
        d = ''
        counterd = 0
        while counterd < 4:
            d += c
            counterd += 1
            c += b
            counterc += 1
        b += a
        counterb += 1
e = (d, d, d)
f, g, h = e
i = f + g + h
j = i + '3'
k_list = [j for _ in range(4)]
l = random.choice(k_list)
m = [l for _ in range(8)]
random.shuffle(m)
n = random.choice(m)
o = ''
for _ in range(5):
    for __ in range(2):
                o += n
p = o[0:]
q = (p, p, p)
r, s, t = q
u = r + s + t
v_dict = {48: u, 21: u, 92: u, 40: u, 25: u, 82: u, 37: u, 4: u}
w_dict = {11: v_dict, 60: v_dict, 90: v_dict, 29: v_dict, 64: v_dict, 13: v_dict, 59: v_dict, 36: v_dict, 8: v_dict}
x = random.choice(list(w_dict.values()))
y = random.choice(list(x.values()))
if y == y:
    ab = y + 'c1'
elif y == '15':
    ab = z + 'c2'
else:
    ab = aa + 'c3'
ac = f'string {ab}'
ad_dict = {26: ac, 75: ac, 66: ac, 30: ac, 27: ac, 46: ac, 57: ac, 17: ac, 9: ac, 25: ac}
ae_dict = {39: ad_dict, 100: ad_dict, 46: ad_dict, 81: ad_dict, 23: ad_dict}
af = random.choice(list(ae_dict.values()))
ag = random.choice(list(af.values()))
ah = ''
for _ in range(3):
    ai = ''
    for _ in range(4):
        ai += ah
        ah += ag
aj = (ai, ai, ai)
ak, al, am = aj
an = ak + al + am
ao = (an, an, an)
ap, aq, ar = ao
at = ap + aq + ar
au = ''
for _ in range(4):
    for __ in range(2):
                au += at
av = f'string {au}'
aw_set = {av, av}
aw = random.choice(list(aw_set))
print(aw)