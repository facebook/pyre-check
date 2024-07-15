# [] 
print(input())

# [1]
a = input()
print(a)

# [1,2]
a = input()
def f1():
    return a
print(f1())

# [1,2,2]
a = input()
def f1():
    return a
def f2():
    return f1()
print(f2())

# [1,2,2,1]
a = input()
def f1():
    return a
def f2():
    return f1()
b = f2()
print(b)

# [1,2,2,1,3]
a = input()
def f1():
    return a
def f2():
    return f1()
b = f2()
def f3():
    if 12 <= 33:
        return b
    elif 33 < 12 <= 66:
         return b
    else:
        return b
print(f3())

# [1,2,2,1,3,2]
a = input()
def f1():
    return a
def f2():
    return f1()
b = f2()
def f3():
    if 4 <= 33:
        return b
    elif 33 < 4 <= 66:
         return b
    else:
        return b
def f4():
    return f3()
print(f4())


# [1,2,2,1,3,2,3]
a = input()
def f1():
    return a
def f2():
    return f1()
b = f2()
def f3():
    if 9 <= 33:
        return b
    elif 33 < 9 <= 66:
         return b
    else:
        return b
def f4():
    return f3()
def f5():
    if 65 <= 33:
        return f4()
    elif 33 < 65 <= 66:
         return f4()
    else:
        return f4()
print(f5())

# all flow is valid 