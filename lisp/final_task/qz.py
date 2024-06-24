import numpy as np

def gram_schmidt_qr(A):
    """ Perform QR decomposition using Gram-Schmidt process """
    (m, n) = A.shape
    Q = np.zeros((m, n))
    R = np.zeros((n, n))
    
    for j in range(n):
        v = A[:, j]
        for i in range(j):
            R[i, j] = np.dot(Q[:, i].T, A[:, j])
            v = v - R[i, j] * Q[:, i]
        R[j, j] = np.linalg.norm(v)
        Q[:, j] = v / R[j, j]
    
    return Q, R

def rq_decomposition(B):
    """ Perform RQ decomposition """
    B_flipped = np.fliplr(np.flipud(B))
    Q, R = gram_schmidt_qr(B_flipped.T)
    R = np.fliplr(np.flipud(R.T))
    Q = np.fliplr(np.flipud(Q.T))
    return R, Q

def qz_iteration(A, B):
    """ Perform one iteration of the QZ algorithm """
    n = A.shape[0]
    
    # Step 1: QR decomposition of A_{i-1} * B_{i-1}^(-1)
    B_inv = np.linalg.inv(B)
    product = np.dot(A, B_inv)
    Q, R = gram_schmidt_qr(product)
    
    # Step 2: Update B and A
    B = np.dot(Q.T, B)
    A = np.dot(Q.T, A)
    
    # Step 3: RQ decomposition of B_tilde
    Q_2, Z = rq_decomposition(B)
    
    # Step 4: Update A
    A = np.dot(A, Z.T)
    B = np.dot(B, Z.T)
    
    return A, B, Q, Z.T

def qz_decomposition(A, B, max_iterations=1000, tol=1e-6):
    """ Perform QZ decomposition """
    n = A.shape[0]
    Q_total = np.eye(n)
    Z_total = np.eye(n)
    
    for i in range(max_iterations):
        A_prev, B_prev = A, B
        A, B, Q, Z = qz_iteration(A, B)
        
        # Accumulate transformations
        Q_total = np.dot(Q_total, Q)
        Z_total = np.dot(Z_total, Z)
        
        # Check for convergence
        # if np.allclose(A, A_prev, atol=tol) and np.allclose(B, B_prev, atol=tol):
        #     break
    
    return A, B, Q_total, Z_total

# Example matrices
#A = np.array([[1, 2, 3], [4, 5, 6], [7, 8, 10]], dtype=float)
# #B = np.array([[3, 2, 1], [1, 1, 1], [2, 1, 1]], dtype=float)
#B = np.array([[3, 2, 1], [5, 2, 4], [2, 3, 6]], dtype=float)

#A = np.random.rand(10, 10)
#B = np.random.rand(10, 10)


# A = np.array([[1, 2, 3, 4, 5],
#                [6, 7, 8, 1, 1],
#                [11, 2, 7, 14, 15],
#                [1, 17, 18, 12, 20],
#                [9, 24, 23, 6, 25]])

# B = np.array([[5, 4, 5, 2, 1],
#                [2, 2, 2, 4, 5],
#                [8, 14, 3, 2, 1],
#                [1, 2, 9, 6, 15],
#                [15, 3, 3, 9, 18]])

def generate_non_singular_matrix(size, min_val=-10, max_val=10):
    while True:
        matrix = np.random.randint(min_val, max_val, (size, size))
        if np.linalg.det(matrix) != 0:
            return matrix

# Пример использования:
size = 100
A = generate_non_singular_matrix(size)
B = generate_non_singular_matrix(size)
# A = np.array([[1, 2, 3, 4, 5],
#                [6, 7, 8, 1, 1],
#                [0, 2, 7, 14, 15],
#                [0, 0, 18, 12, 20],
#                [0, 0, 0, 6, 25]])

# B = np.array([[5, 4, 5, 2, 1],
#                [0, 2, 2, 4, 5],
#                [0, 0, 3, 2, 1],
#                [0, 0, 0, 6, 15],
#                [0, 0, 0, 0, 18]])

# Perform QZ decomposition
A_final, B_final, Q, Z = qz_decomposition(A, B)

print("Final A (generalized Schur form):\n", A_final)
print("Final B (generalized Schur form):\n", B_final)
print("Accumulated Q:\n", Q)
print("Accumulated Z:\n", Z)



def check_qz_decomposition(A, B, Q, Z, S, T, tol=1e-4):
    # Проверяем ортогональность матриц Q и Z
    Q_orthogonal = np.allclose(np.dot(Q, Q.T), np.eye(Q.shape[0]), atol=tol)
    Z_orthogonal = np.allclose(np.dot(Z, Z.T), np.eye(Z.shape[0]), atol=tol)
    
    # Проверяем разложения Q^T A Z = S и Q^T B Z = T
    S_check = np.allclose(np.dot(np.dot(Q.T, A), Z), S, atol=tol)
    T_check = np.allclose(np.dot(np.dot(Q.T, B), Z), T, atol=tol)
    
    results = {
        'Q_orthogonal': Q_orthogonal,
        'Z_orthogonal': Z_orthogonal,
        'S_check': S_check,
        'T_check': T_check,
    }
    
    return results


results = check_qz_decomposition(A, B, Q, Z, A_final, B_final)
print(results)

print("QSZ: ", np.dot(np.dot(Q, A_final), Z.T))
print("QTZ: ", np.dot(np.dot(Q, B_final), Z.T))

# import numpy as np

# def generate_non_singular_matrix(size, min_val=-10, max_val=10):
#     while True:
#         matrix = np.random.randint(min_val, max_val, (size, size))
#         if np.linalg.det(matrix) != 0:
#             return matrix

# # Пример использования:
# size = 20
# matrix = generate_non_singular_matrix(size)
# print(matrix)
