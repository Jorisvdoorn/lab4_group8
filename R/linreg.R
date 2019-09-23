linreg<-setRefClass("linreg",
                    fields = list(
                      betahat = "numeric", # regressions coefficients
                      yhat = "numeric", # fitted values
                      reshat = "numeric", # residuals
                      df = "numeric", # degrees of freedom
                      resvar = "numeric", # residual variance
                      betavar = "numeric", # variance of the regression coefficients
                      betat = "numeric", # t-values for each coefficient
                    ),
                    methods = list(
                      print = function(x) {something}, # does something
                      plot = function(x) {somethingelse}, # does something else
                      resid = function(x) {somethingelse}, # does something else
                      pred = function(x) {somethingelse}, # does something else
                      coef = function(x) {somethingelse}, # does something else
                      summary = function(x) {somethingelse}, # does something else
                    )
)

linreg<-function(formula, data){
    stopifnot()
}