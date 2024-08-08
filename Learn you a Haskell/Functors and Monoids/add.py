import sys

def addMe(a: float, b: float) -> float:
    return a + b

x, y = sys.argv[1], sys.argv[2]

Sum =addMe(x, y)
print(Sum)
