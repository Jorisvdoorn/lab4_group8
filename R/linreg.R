linreg = function(formula, data){
  construct_linreg <- setRefClass(Class = "linreg",
                                  
                                  fields = list(
                                    formula = "formula",
                                    data = "data.frame",
                                    data_name = "character",
                                    formula_name = "character",
                                    X = "matrix",
                                    Y = "matrix",
                                    Q = "matrix",
                                    R = "matrix",
                                    beta_hat = "matrix", # regressions coefficients
                                    y_hat = "matrix", # fitted values
                                    res_hat = "matrix", # residuals
                                    df = "numeric", # degrees of freedom
                                    res_var = "numeric", # residual variance
                                    var_beta = "matrix", # variance of the regression coefficients
                                    t_beta = "matrix", # t-values for each coefficient
                                    std_error = "matrix", # standard error
                                    p_value = "matrix" # significance level // p-value
                                  ),
                                  
                                  methods = list(
                                    # initialise class and assigns values with <<-
                                    initialize = function(formula, data) {
                                      
                                      # Assigning the data to matrices of dependent (Y) and independent vars (X)
                                      X <<- model.matrix(formula, data = data) 
                                      Y <<- as.matrix(data[all.vars(formula)[1]])
                                      
                                      # Calculating beta_hat through QR decomp 
                                      decomp = qr(X)
                                      Q <<- qr.Q(decomp)
                                      R <<- qr.R(decomp)
                                      beta_hat <<- solve(R) %*% t(Q) %*% Y
                                      
                                      # fitted values
                                      y_hat <<- X %*% beta_hat
                                      
                                      # residuals
                                      res_hat <<- Y - y_hat
                                      
                                      # degrees of freedom
                                      df <<- nrow(X)-ncol(X)
                                      
                                      # residual variance
                                      res_var <<- as.numeric((t(res_hat) %*% res_hat)/df)
                                      
                                      # variance of the regression coefficients
                                      var_beta <<- solve(t(R) %*% R) * res_var^2
                                      
                                      # t-values for each coefficient
                                      t_beta <<- beta_hat / sqrt(as.numeric(var(beta_hat)))
                                      
                                      # standard error
                                      std_error <<- sqrt(res_var) / sqrt(nrow(X))
                                      
                                      # p-value
                                      p_value <<- pt(t_beta, df)
                                      
                                      data_name <<- deparse(substitute(data))
                                      formula_name <<- deparse(formula)
                                    },
                                    
                                    # print out the coefficients and coefficient names
                                    print = function(){
                                      cat("Call:\n")
                                      cat("linreg(formula = ", formula_name, ", data = ",data_name,")\n\n")
                                    },
                                    
                                    plot = function(){
                                      ggplot(data, aes(x=y_hat, y=res_hat))
                                    },
                                    
                                    resid = function(){
                                      return(res_hat)
                                    },
                                    
                                    pred = function(){
                                      return(y_hat)
                                    },
                                    
                                    coef = function(){
                                      return(beta_hat)
                                    },
                                    
                                    summary = function(){
                                      df1 <- data.frame(beta_hat, std_error, t_beta, p_value)
                                      return(df1)
                                    }
                                  )
  )
  
  return(construct_linreg$new(formula, data))
  
}

linreg1 <- linreg(Petal.Length~Species, iris)
linreg1$resid()
