#' Running a Linear Multiple Reregression Model.
#' 
#' @param formula Takes a formula of the form Y~X.
#' @param data Takes a dataset in the form of a data.frame.
#' 
#' @return Returns an object of the class 'linreg'. This object can be manipulated.

linreg = function(formula, data){
  data_name = deparse(substitute(data))
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
                                    p_value = "matrix", # significance level // p-value
                                    std_dev_res = "numeric", # the standard deviation of the residuals
                                    std_res_sqrt = "matrix" # square root of the absolute standardized residuals
                                  ),
                                  
                                  methods = list(
                                    # initialise class and assigns values with <<-
                                    initialize = function(formula, data, data_name) {
                                      
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
                                      var_beta <<- solve(t(R) %*% R) * res_var
                                      
                                      # standard error
                                      std_error <<- as.matrix(sqrt(diag(var_beta)))
                                      
                                      # t-values for each coefficient
                                      t_beta <<- beta_hat / std_error
                                      
<<<<<<< HEAD
                                      #p_values
                                      p_value <<- 2*pt(t_beta, df = df, lower.tail = FALSE)
=======
                                      # p-value
                                      p_value <<- pt(t_beta, df)
                                      
                                      # standard deviation of residuals
                                      std_dev_res <<- sqrt(as.numeric((t(res_hat) %*% res_hat)/df))
                                      
                                      # square root of the absolute standardized residuals
                                      std_res_sqrt <<- sqrt(abs(res_hat/std_dev_res))
>>>>>>> d94870e6d063972d18fc12258b2cf134edf48b6c
                                      
                                      data_name <<- data_name
                                      formula_name <<- deparse(formula)
                                    },
                                    
                                    # print out the coefficients and coefficient names
                                    print = function(){
                                      
                                      beta_vector = as.vector(beta_hat)
                                      names(beta_vector) = rownames(beta_hat)
                                      cat("Call:\n")
                                      cat(paste("linreg(formula = ", formula_name, ", data = ",data_name,")", sep = ""),"\n\n")
                                      cat("Coefficients:\n")
                                      print_inside(beta_vector)
                                    },
                                    
                                    plot = function(){
                                      graph1 <- ggplot(data, aes(x=y_hat, y=res_hat)) +
                                        geom_point() +
                                        ylab("Residuals") +
                                        xlab("Fitted values") +
                                        geom_smooth(method = "lm", formula = y ~ x) +
                                        ggtitle("Residuals vs Fitted")
                                      
                                      graph2 <- ggplot(data, aes(x=y_hat, y=std_res_sqrt)) +
                                        geom_point() +
                                        ylab(expression(sqrt("|Standardized residuals|"))) +
                                        xlab("Fitted values") +
                                        geom_smooth(method = "lm", formula = y ~ x) +
                                        ggtitle("Scale-Location")
                                      
                                      graph_list<-grid.arrange(graph1,graph2)
                                      return(graph_list)
                                    },
                                    
                                    resid = function(){
                                      return(res_hat)
                                    },
                                    
                                    pred = function(){
                                      return(y_hat)
                                    },
                                    
                                    coef = function(){
                                      beta_vector = as.vector(beta_hat)
                                      names(beta_vector) = rownames(beta_hat)
                                      return(beta_vector)
                                    },
                                    
                                    summary = function(){
                                      cat("Call:\n")
                                      cat(paste("linreg(formula = ", formula_name, ", data = ",data_name,")", sep = ""),"\n\n")
<<<<<<< HEAD
                                      cat("Coefficients:\n")
                                      
=======
                                      cat(paste("Std Dev of Residuals = ", std_dev_res, "\nDegrees of Freedom = ", df,"\n\n"))
                                      df1 <- data.frame(beta_hat, std_error, t_beta, p_value)
                                      return(df1)
>>>>>>> d94870e6d063972d18fc12258b2cf134edf48b6c
                                    }
                                  )
  )
  return(construct_linreg$new(formula, data, data_name))
}

print_inside = function(x, ...){
  print(x, ...)
}

create_stars = function(df){
  stars = matrix(0, nrow = length(df), ncol = 1)
  for (i in 1:length(df)){
    if (df[i] < 0.001){stars[i] = "***"}
    else if(df[i] < 0.01){stars[i] = "**"}
    else if(df[i] < 0.05){stars[i] = "*"}
    else if(df[i] < 0.1){stars[i] = "."}
    else {stars[i] = " "}
  }
  return(stars)
}

linreg1 = linreg(Petal.Length~Species, iris)
lm1 = lm(Petal.Length~Species, iris)


beta_round = formatC(round(linreg1$beta_hat, 5), format='f', digits=5)
std_round = formatC(round(linreg1$std_error, 5), format='f', digits=5)
tval_round = formatC(round(linreg1$t_beta, 2), format='f', digits=2)
pval_round = linreg1$p_value
pval_round[which(pval_round<2e-16)] = "<2e-16"
pval_round = formatC(pval_round, format='e', digits=2)
summary_vec = cbind(beta_round, std_round, tval_round, pval_round, create_stars(linreg1$p_value))
colnames(summary_vec) = c("Estimate", "Std. Error", "t value", "Pr(>|t|)", " ")
noquote(summary_vec)
