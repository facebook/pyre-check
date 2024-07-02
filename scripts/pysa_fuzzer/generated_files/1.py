import random
import math
a = input()
b = f'string {a}'
c = b[0:]
d = (c, c, c)
e, f, g = d
h = e + f + g
i_dict = {72: h, 44: h, 58: h, 70: h, 14: h, 59: h, 31: h, 51: h, 1: h}
j_dict = {47: i_dict, 100: i_dict, 67: i_dict, 57: i_dict, 67: i_dict}
k_dict = {42: j_dict, 96: j_dict, 35: j_dict, 97: j_dict, 35: j_dict, 33: j_dict}
l = random.choice(list(k_dict.values()))
m = random.choice(list(l.values()))
n = random.choice(list(m.values()))
o_list = [n for _ in range(7)]
p_list = [o_list for _ in range(3)]
q = random.choice(p_list)
r = random.choice(q)
s = ''
counters = 0
while counters < 3:
    s += r
    counters += 1
t_list = [s for _ in range(8)]
u = random.choice(t_list)
v_dict = {45: u, 98: u, 63: u, 61: u, 95: u, 31: u, 81: u, 97: u, 6: u}
w_dict = {3: v_dict, 47: v_dict, 49: v_dict, 21: v_dict, 92: v_dict, 66: v_dict, 31: v_dict, 99: v_dict}
x = random.choice(list(w_dict.values()))
y = random.choice(list(x.values()))
z_list = [y for _ in range(3)]
aa = random.choice(z_list)
ab = ''
for _ in range(10):
        if _ == 3:
            continue
        ab += aa
ac_dict = {22: ab, 51: ab, 88: ab, 36: ab, 66: ab, 99: ab}
ad = random.choice(list(ac_dict.values()))
ae = ''
for _ in range(3):
    ae += ad
af = ae + '1'
ag = af + '2'
ah_list = [ag for _ in range(3)]
ai_list = [ah_list for _ in range(5)]
aj_list = [ai_list for _ in range(2)]
ak = random.choice(aj_list)
al = random.choice(ak)
am = random.choice(al)
an_list = [am for _ in range(6)]
ao_list = [an_list for _ in range(9)]
ap_list = [ao_list for _ in range(6)]
aq = random.choice(ap_list)
ar = random.choice(aq)
at = random.choice(ar)
au = at[0:]
av = (au, au, au)
aw, ax, ay = av
az = aw + ax + ay
ba = [az for _ in range(9)]
random.shuffle(ba)
bb = random.choice(ba)
print(bb)