import math

def sign(x):
    """Функция, возвращающая знак числа."""
    return 1 if x >= 0 else -1

def norm(x):
    """Функция для вычисления Евклидовой нормы вектора."""
    return sum(i**2 for i in x)**0.5

def transpose(matrix):
    """Функция для транспонирования матрицы."""
    return [[matrix[j][i] for j in range(len(matrix))] for i in range(len(matrix[0]))]

def matrix_mult(A, B):
    """
    Выполняет матричное умножение A и B.
    """
    n = len(A)
    result = [[0] * n for _ in range(n)]
    for i in range(n):
        for j in range(n):
            for k in range(n):
                result[i][j] += A[i][k] * B[k][j]
    return result

def print_matrix_with_rounding(matrix):
    """
    Prints the given matrix with each element rounded to two decimal places.

    :param matrix: The input matrix (list of lists).
    """
    for row in matrix:
        rounded_row = [f"{elem:.2f}" for elem in row]
        print(" ".join(rounded_row))

def householder_reflection(x):
    """Функция для вычисления матрицы отражения Householder."""
    v = x.copy()
    v[0] += sign(v[0]) * norm(v)
    norm_v = norm(v)
    for i in range(len(v)):
        v[i] /= norm_v
    identity = [[0 if j != i else 1 for j in range(len(x))] for i in range(len(x))]
    vvT = [[v[i] * v[j] for j in range(len(v))] for i in range(len(v))]
    vvT_div = [[vvT[i][j] / norm_v**2 for j in range(len(vvT[0]))] for i in range(len(vvT))]
    H = [[identity[i][j] - 2 * vvT_div[i][j] for j in range(len(identity[0]))] for i in range(len(identity))]
    return H

def triangularize(A, B):
    """
    Функция для приведения матрицы B к верхнетреугольному виду
    с использованием Householder reflections и применения отражений к матрице A.
    """
    m, n = len(B), len(B[0])
    Q = [[1 if j == i else 0 for j in range(m)] for i in range(m)]  # Единичная матрица для накопления отражений
    for k in range(n - 1):  # k от 0 до n-2
        x = [B[i][k] for i in range(k, m)]  # Выбираем столбец k
        H = [[1 if j == i else 0 for j in range(m)] for i in range(m)]  # Единичная матрица для отражения
        H_k = householder_reflection(x)  # Вычисляем Householder reflection
        for i in range(k, m):
            for j in range(k, m):
                H[i][j] = H_k[i - k][j - k]
        B = matrix_mult(H, B)  # Применяем отражение к B
        Q = matrix_mult(H, Q)  # Обновляем матрицу отражений Q
        A = matrix_mult(A, transpose(H))  # Применяем транспонированное отражение к A
    return A, B


def givens_coeffs(a, b):
    if b == 0:
        c = 1
        s = 0
    elif abs(b) > abs(a):
        t = -a / b
        s = 1 / math.sqrt(1 + t**2)
        c = s * t

    else:
        t = -b / a 
        c = 1 / math.sqrt(1 + t**2)
        s = c * t

    return c, s

def givens(A, i, k, c, s):
    n = len(A)
    for j in range(n):
        t_1 = A[j][i]
        t_2 = A[j][k]
        A[j][i] = c * t_1 - s * t_2
        A[j][k] = s * t_1 + c * t_2
    return A

def givens_transpose(A, i, k, c, s):
    n = len(A)
    for j in range(n):
        t_1 = A[i][j]
        t_2 = A[k][j]
        A[i][j] = c * t_1 - s * t_2
        A[k][j] = s * t_1 + c * t_2
    return A

def givens_test(A, i, k):
    c, s = givens_coeffs(A[i-1][k], A[i][k])
    A = givens_transpose(A, i-1, i, c, s)
    return A



def givens_rotation(A, B):
    n = len(A)
    for j in range(n-2):
        for i in range(n - 1, j + 1, -1):
            [c, s] = givens_coeffs(A[i - 1][j], A[i][j])
            A = givens_transpose(A, i-1, i, c, s)
            B = givens_transpose(B, i-1, i, c, s)

            [c, s] = givens_coeffs(-B[i][i], B[i][i-1])
            A = givens(A, i-1, i, c, s)
            B = givens(B, i-1, i, c, s)


    return A, B


# Пример использования
A = [[1, 2, 3, 4, 5], 
     [6, 7, 8, 9, 10], 
     [11, 12, 13, 14, 15], 
     [16, 17, 18 ,19 ,20], 
     [21, 22, 23, 24, 25]]

B = [[1, 22, 6, 11, 23], 
     [3, 7, 1, 9, 1], 
     [2, 11, 1, 4, 1], 
     [14, 6, 2 ,9 ,20], 
     [11, 2, 3, 4, 15]]

# A = [[1, 2, 3], [4, 5, 6], [7, 8 ,9]]
# B = [[4, 2, 1], [2, 3, 6], [1, 2 ,9]]

# A = [
#     [0.5, 1.96, 3.0], 
#     [2.0, 4.9, 6.0], 
#     [3.5, 7.84, 9.0]]

# B = [[0.5, 1.0, 1.5], 
#      [0.0, 4.9, 5.88], 
#      [0.0, 0.0, 9.0]]

C = [[1, 2, 3, 4, 5], [1, 2, 3, 4, 5], [1, 2, 3, 4, 5], [1, 2, 3, 4, 5], [1, 2, 3, 4, 5]]

A_triangularized, B_triangularized = triangularize(A, B)

print("Приведенная к верхнетреугольному виду матрица A:")
print_matrix_with_rounding(A_triangularized)
print("\nПриведенная к верхнетреугольному виду матрица B:")
print_matrix_with_rounding(B_triangularized)


# A_giv = givens_rotation(A_triangularized, B_triangularized)

# A_giv = givens_test(A, 2, 1)
# print("\nЗануление A:")
# for row in A_giv:
#     print(row)



A_givens, B_givens = givens_rotation(A_triangularized, B_triangularized)
print("\nHessinberged A:")
print_matrix_with_rounding(A_givens)

print("\nTriangular B:")
print_matrix_with_rounding(B_givens)


