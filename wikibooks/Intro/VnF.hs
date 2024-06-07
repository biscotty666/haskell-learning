area r = pi * r ^ 2
calcHfM12 n = n / 2 - 12
areaRect l w = l * w
areaSquare s = areaRect s s
areaTriangle b h = 0.5 * b * h
double x = x * 2
quadruple x = double (double x)
heron a b c = sqrt (s * (s - a) * (s - b) * (s - c))
  where
  s = (a + b + c) / 2
areaTriangleTrig a b c = c * height / 2
  where
  cosa = (b ^ 2 + c ^ 2 - a ^ 2 ) / (2 * b * c)
  sina = sqrt (1 - cosa ^ 2)
  height = b * sina
