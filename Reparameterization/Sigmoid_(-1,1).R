# If we have the constraint of (-1,1) then we can also use the sigmoid function:

# Here the guess, x, is inserted into the inverted sigmoid function:
x_tilde <- log((1+x)/(1-x))

# And we can then use in the sigmoid function
x <- (2*exp(x)/(1+exp(x)) - 1)

# Which insures that we remain between -1 and 1
