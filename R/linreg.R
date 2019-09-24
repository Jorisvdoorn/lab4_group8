linreg <- setRefClass("linreg",
                      fields = list(
                        formula = "formula",
                        data = "data.frame",
                        beta_hat = "numeric", # regressions coefficients
                        y_hat = "numeric", # fitted values
                        res_hat = "numeric", # residuals
                        df = "numeric", # degrees of freedom
                        res_var = "numeric", # residual variance
                        beta_var = "numeric", # variance of the regression coefficients
                        beta_t = "numeric" # t-values for each coefficient
                      ),
                      methods = list(
                        constructor = function() {
                          # initialise class and assigns values with <<-
                          beta_hat <<- 1      # regressions coefficients
                          y_hat <<- 1         # fitted values
                          res_hat <<- 1       # residuals
                          df <<- 1            # degrees of freedom
                          res_var <<- 1       # residual variance
                          beta_var <<- 1      # variance of the regression coefficients
                          beta_t <<- 1        # t-values for each coefficient
                        }
                        
                        reg_output = function(formula, data){
                          # checking input
                          stopifnot(is.data.frame(data))
                          
                          # assigning dependent and independent variables
                          mod_mat <- model.matrix(formula, data = data) # transforming dataframe to matrix
                          temp <- all.vars(formula) # extracting all variables
                          dep_var <- temp[1] # assiging dependent variable
                          
                          # calculating beta_hat using QR-decomposition
                          decomp = qr(mod_mat)
                          Q = qr.Q(decomp)
                          R = qr.R(decomp)
                          rhs = t(Q) %*% dep_var
                          beta_hat <- solve(R) %*% rhs
                          
                          
                          
                          # calculate parameters and store into object of class linreg
                          
                          
                          # return object
                          return(l1)
                        }
                        
                        print = function(x) {something}, # does something
                        
                        plot = function(x) {somethingelse}, # does something else
                        
                        resid = function(x) {somethingelse}, # does something else
                        
                        pred = function(x) {somethingelse}, # does something else
                        
                        coef = function(x) {somethingelse}, # does something else
                        
                        summary = function(x) {somethingelse} # does something else
                      )
)

