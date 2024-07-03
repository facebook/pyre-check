import random
import math
a = input()
b = f'string {a}'
c = ''
counterc = 0
while counterc < 2:
    d = ''
    counterd = 0
    while counterd < 2:
        e = ''
        countere = 0
        while countere < 2:
            e += d
            countere += 1
            d += c
            counterd += 1
        c += b
        counterc += 1
f_set = {e, e, e, e, e, e, e}
f = random.choice(list(f_set))
g_dict = {72: f, 62: f, 100: f, 50: f, 90: f}
h_dict = {49: g_dict, 71: g_dict, 34: g_dict, 87: g_dict, 86: g_dict}
i = random.choice(list(h_dict.values()))
j = random.choice(list(i.values()))
k = j[0:]
l_list = [k for _ in range(10)]
m = random.choice(l_list)
n = ''
countern = 0
while countern < 4:
    o = ''
    countero = 0
    while countero < 3:
        o += n
        countero += 1
        n += m
        countern += 1
p = o + '.'
q = ''
for _ in range(4):
    r = ''
    for _ in range(2):
        s = ''
        for _ in range(4):
            s += r
            r += q
        q += p
t_dict = {42: s, 22: s, 97: s, 23: s, 41: s, 4: s, 64: s, 69: s, 23: s}
u_dict = {76: t_dict, 87: t_dict, 19: t_dict, 75: t_dict, 35: t_dict, 78: t_dict}
v_dict = {17: u_dict, 21: u_dict, 84: u_dict, 7: u_dict, 82: u_dict, 86: u_dict, 7: u_dict, 28: u_dict}
w = random.choice(list(v_dict.values()))
x = random.choice(list(w.values()))
y = random.choice(list(x.values()))
z_dict = {14: y, 8: y}
aa_dict = {6: z_dict, 31: z_dict, 27: z_dict, 20: z_dict}
ab = random.choice(list(aa_dict.values()))
ac = random.choice(list(ab.values()))
ad = ac[0:]
def ae():
    return ad
def af():
    return ae()
ag = af()
ah_set = {ag, ag, ag, ag}
ah = random.choice(list(ah_set))
ai = ''
for _ in range(6):
        if _ == 3:
            continue
        ai += ah
aj = f'string {ai}'
ak = ''
for _ in range(5):
    for __ in range(3):
                ak += aj
al = ak + '9'
am = al + '3'
print(am)