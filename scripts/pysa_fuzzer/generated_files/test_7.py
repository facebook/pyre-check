import random
import math

a = input()
b = ''
for _ in range(5):
    for __ in range(2):
                b += a
c = b[0:]
d = c + '7'
e = d + '3'
f = ''
for _ in range(5):
    g = ''
    for _ in range(3):
        g += f
        f += e
h_dict = {92: g, 56: g, 89: g, 96: g, 66: g, 44: g, 37: g, 66: g}
i = random.choice(list(h_dict.values()))
j = ''
for _ in range(5):
    for __ in range(3):
                j += i
k = (j, j, j)
l, m, n = k
o = l + m + n
p = o + '4'
q = p + '5'
r = [q for _ in range(5)]
random.shuffle(r)
s = random.choice(r)
t = (s, s, s)
u, v, w = t
x = u + v + w
y = x + '.'
z_list = [y for _ in range(8)]
aa_list = [z_list for _ in range(7)]
ab = random.choice(aa_list)
ac = random.choice(ab)
ad = ''
for _ in range(2):
    for __ in range(4):
                ad += ac
ae_set = {ad, ad, ad, ad, ad, ad, ad, ad, ad, ad}
ae = random.choice(list(ae_set))
af_dict = {59: ae, 7: ae, 37: ae, 70: ae, 75: ae}
ag_dict = {9: af_dict, 25: af_dict, 33: af_dict, 9: af_dict, 64: af_dict, 11: af_dict, 8: af_dict, 70: af_dict, 47: af_dict, 15: af_dict}
ah = random.choice(list(ag_dict.values()))
ai = random.choice(list(ah.values()))
aj = ai[0:]
ak = aj[0:]
al = ''
counteral = 0
while counteral < 5:
    al += ak
    counteral += 1
print(al)