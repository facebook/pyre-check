import random
import math
a = input()
b_dict = {32: a, 39: a, 8: a, 21: a}
c_dict = {38: b_dict, 80: b_dict, 69: b_dict, 15: b_dict, 32: b_dict}
d_dict = {42: c_dict, 29: c_dict, 3: c_dict, 1: c_dict, 53: c_dict}
e_dict = {97: d_dict, 75: d_dict, 43: d_dict, 5: d_dict}
f_dict = {42: e_dict, 88: e_dict, 41: e_dict, 85: e_dict, 85: e_dict, 13: e_dict}
g = random.choice(list(f_dict.values()))
h = random.choice(list(g.values()))
i = random.choice(list(h.values()))
j = random.choice(list(i.values()))
k = random.choice(list(j.values()))
print(k)
print(f_dict)