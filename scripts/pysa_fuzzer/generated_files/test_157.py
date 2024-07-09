import random
import math

a = input()
b_list = [a for _ in range(6)]
c_list = [b_list for _ in range(2)]
d_list = [c_list for _ in range(8)]
e = random.choice(d_list)
f = random.choice(e)
g = random.choice(f)
def h():
    return g
i = h()
def j():
    return i
def k():
    return j()
def l():
    return k()
m = l()
def n():
    return m
def o():
    return n()
p = o()
if p == p:
    s = p + 'c1'
elif p == '19':
    s = q + 'c2'
else:
    s = r + 'c3'
t = ''
for _ in range(5):
        if _ == 2:
            break
        t += s
u_dict = {4: t, 39: t, 32: t, 48: t, 91: t, 55: t, 13: t, 12: t, 8: t}
v_dict = {24: u_dict, 1: u_dict, 69: u_dict, 69: u_dict, 52: u_dict, 22: u_dict, 58: u_dict, 42: u_dict, 8: u_dict}
w_dict = {4: v_dict, 22: v_dict, 98: v_dict, 61: v_dict, 44: v_dict, 41: v_dict, 93: v_dict, 33: v_dict}
x = random.choice(list(w_dict.values()))
y = random.choice(list(x.values()))
z = random.choice(list(y.values()))
aa = z + '.'
ab_list = [aa for _ in range(3)]
ac = random.choice(ab_list)
ad_set = {ac, ac, ac, ac, ac, ac, ac}
ad = random.choice(list(ad_set))
ae_list = [ad for _ in range(2)]
af_list = [ae_list for _ in range(2)]
ag_list = [af_list for _ in range(3)]
ah = random.choice(ag_list)
ai = random.choice(ah)
aj = random.choice(ai)
ak = aj + '.'
al = f'string {ak}'
am_dict = {50: al, 4: al, 28: al, 63: al, 6: al, 11: al, 20: al, 56: al, 46: al}
an = random.choice(list(am_dict.values()))
ao_set = {an, an, an, an, an, an}
ao = random.choice(list(ao_set))
ap = [ao for _ in range(9)]
random.shuffle(ap)
aq = random.choice(ap)
ar_dict = {44: aq, 59: aq}
at_dict = {86: ar_dict, 12: ar_dict, 71: ar_dict, 39: ar_dict, 40: ar_dict, 2: ar_dict}
au_dict = {40: at_dict, 63: at_dict, 17: at_dict}
av = random.choice(list(au_dict.values()))
aw = random.choice(list(av.values()))
ax = random.choice(list(aw.values()))
ay = f'string {ax}'
print(ay)