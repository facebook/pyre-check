import random
import math
a = input()
b_list = [a for _ in range(6)]
c_list = [b_list for _ in range(9)]
d_list = [c_list for _ in range(3)]
e = random.choice(d_list)
f = random.choice(e)
g = random.choice(f)
h_list = [g for _ in range(3)]
i_list = [h_list for _ in range(2)]
j_list = [i_list for _ in range(2)]
k = random.choice(j_list)
l = random.choice(k)
m = random.choice(l)
n = m[0:]
o = ''
countero = 0
while countero < 2:
    p = ''
    counterp = 0
    while counterp < 4:
        q = ''
        counterq = 0
        while counterq < 4:
            q += p
            counterq += 1
            p += o
            counterp += 1
        o += n
        countero += 1
r = q + '.'
s_list = [r for _ in range(5)]
t_list = [s_list for _ in range(2)]
u_list = [t_list for _ in range(2)]
v = random.choice(u_list)
w = random.choice(v)
x = random.choice(w)
y = ''
countery = 0
while countery < 4:
    y += x
    countery += 1
z = y + '.'
aa = f'string {z}'
ab_dict = {77: aa, 42: aa, 56: aa, 32: aa, 38: aa}
ac_dict = {48: ab_dict, 37: ab_dict, 19: ab_dict, 75: ab_dict, 61: ab_dict}
ad_dict = {10: ac_dict, 78: ac_dict, 73: ac_dict, 25: ac_dict, 41: ac_dict, 12: ac_dict, 66: ac_dict, 13: ac_dict, 27: ac_dict, 65: ac_dict}
ae = random.choice(list(ad_dict.values()))
af = random.choice(list(ae.values()))
ag = random.choice(list(af.values()))
ah = ''
for _ in range(5):
    ai = ''
    for _ in range(3):
        ai += ah
        ah += ag
aj = ai + '1'
ak = ''
for _ in range(5):
        if _ == 5:
            continue
        ak += aj
al = ''
for _ in range(3):
    am = ''
    for _ in range(4):
        am += al
        al += ak
an = am + '.'
ao = f'string {an}'
def ap():
    return ao
aq = ap()
ar = [aq for _ in range(7)]
random.shuffle(ar)
at = random.choice(ar)
print(at)