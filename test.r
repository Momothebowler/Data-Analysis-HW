A <- matrix(

    # Taking sequence of elements
    c(
        0, 1, 0, 0, 0, 0, 0, 0,
        2 / 8, 0, 1 / 8, 1 / 8, 1 / 8, 1 / 8, 1 / 8, 1 / 8,
        1 / 8, 0, 2 / 8, 1 / 8, 1 / 8, 1 / 8, 1 / 8, 1 / 8,
        1 / 8, 0, 1 / 8, 2 / 8, 1 / 8, 1 / 8, 1 / 8, 1 / 8,
        1 / 8, 0, 1 / 8, 1 / 8, 2 / 8, 1 / 8, 1 / 8, 1 / 8,
        1 / 8, 0, 1 / 8, 1 / 8, 1 / 8, 2 / 8, 1 / 8, 1 / 8,
        1 / 8, 0, 1 / 8, 1 / 8, 1 / 8, 1 / 8, 2 / 8, 1 / 8,
        1 / 8, 0, 1 / 8, 1 / 8, 1 / 8, 1 / 8, 1 / 8, 2 / 8
    ),

    # No of rows
    nrow = 8,

    # No of columns
    ncol = 8,

    # By default matrices are in column-wise order
    # So this parameter decides how to arrange the matrix
    byrow = TRUE
)

B <- A

# for (x in 1:100) {
#    B <- B %*% A
# }
print(B)
B <- B[-c(1), ]
print(B)
B <- B[, c(2, 3, 4, 5, 6, 7, 8)]
print(B)
one <- diag(1, 7, 7)
i <- c(1, 1, 1, 1, 1, 1, 1)
print((solve(one - B)) %*% i)
