import random
import math
a = input()
b = a + '1'
c = b + '8'
d = c + '9'
e = (d, d, d)
f, g, h = e
i = f + g + h
j_set = {i, i, i, i}
j = random.choice(list(j_set))
k = ''
for _ in range(2):
    l = ''
    for _ in range(5):
        l += k
        k += j
m = [l for _ in range(10)]
random.shuffle(m)
n = random.choice(m)
o_dict = {25: n, 5: n, 70: n, 98: n, 60: n, 48: n, 33: n, 60: n, 64: n}
p = random.choice(list(o_dict.values()))
q = p + '.'
r = q + '.'
s_dict = {69: r, 2: r, 43: r, 35: r, 63: r}
t = random.choice(list(s_dict.values()))
u_list = [t for _ in range(2)]
v_list = [u_list for _ in range(5)]
w = random.choice(v_list)
x = random.choice(w)
y = f'string {x}'
z = ''
for _ in range(2):
    for __ in range(3):
                z += y
aa = [z for _ in range(6)]
random.shuffle(aa)
ab = random.choice(aa)
ac = ab + '5'
ad = ac + '4'
ae = ad + '3'
af = ae + '1'
ag = af + '7'
ah_dict = {87: ag, 91: ag, 89: ag}
ai_dict = {15: ah_dict, 20: ah_dict, 4: ah_dict, 76: ah_dict, 9: ah_dict, 42: ah_dict, 61: ah_dict, 34: ah_dict}
aj_dict = {60: ai_dict, 96: ai_dict, 49: ai_dict, 99: ai_dict, 41: ai_dict, 8: ai_dict, 36: ai_dict, 13: ai_dict, 57: ai_dict}
ak = random.choice(list(aj_dict.values()))
al = random.choice(list(ak.values()))
am = random.choice(list(al.values()))
an = [am for _ in range(8)]
random.shuffle(an)
ao = random.choice(an)
ap = f'string {ao}'
print(ap)