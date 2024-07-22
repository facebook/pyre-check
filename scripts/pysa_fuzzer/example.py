# Pattern One: 


def f0():
    if random.randint(1, 3) == 1:
        return input()
    else:
        return f0()
        

def f2():
    if random.randint(1, 3) == 1:
        return f0()
    else:
        return f2()
        

def f1(x):
    if random.randint(1, 3) == 1:
        return print(x)
    else:
        return f1(x)
        

def f3(x):
    if random.randint(1, 3) == 1:
        return f1(x)
    else:
        return f3(x)
        
f3(f2())


# pattern two 

