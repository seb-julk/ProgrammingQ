
double CallR(Function func, double x)
{
    // allows passing R function to cpp double
    SEXP func_val = func(x);
    return *REAL(func_val);
}
