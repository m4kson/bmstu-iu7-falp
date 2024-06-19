import numpy as np

def deflation(A, B):
    pass

def qz_step():
    pass

def qz(A, B, m):
    n = A.shape[0]
    assert A.shape == (n, n) and B.shape == (n, n), "Matrices A and B must be square and the same size"
    assert 2 <= m <= n, "m must by in the range [2, n]"

    Q = np.eye(n) # матрица с единицами на главной диагонали, остальные нули
    Z = np.eye(n)
    i = 0
    l = n

    u = np.finfo(float).eps

    k = i
    tol = 0

    while ((k < l) and (abs(B[k][k]) > u * (abs(B[k][k+1]) + tol))):
        k += 1
        tol = abs(B[k-1][k])

    if (abs(B[l][l]) > u * abs(B[l-1][l])):
        k += 1 #тут сомнительный момент, это вроде нужно т.к. в предыдущем цикле l не покрывается из-за переполнения индекса при l + 1, но тогда если ...

    if (k <= l):
        B[k][k] = 0
        deflation(A, B) 

    k = i

    while ((k < l) and (abs(A[k+1][k]) > u * (abs(A[k][k]) + abs(A[k+1][k+1])))):
        k += 1
    
    if (k < l):
        A[k + 1][k] = 0
        i = k + 1
        if (i + 1 >= l):
            l = i - 1
            i = 1
            if (l <= 2):
                return Q, Z
    
    else:
        qz_step()
            
    


