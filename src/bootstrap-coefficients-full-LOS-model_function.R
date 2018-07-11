

#***************************************************
# FUNCTION TO FIT MODEL M4 FOR BOOTSTRAPPING 
#***************************************************

library("broom")

# function definition: ----------
extract.coeffs.m4_function <- 
    function(data, i){
        # data: is a df
        # i:  is just an iterating index 
        
        require("magrittr")
        require("VGAM")
        require("boot")
        
        m <- vglm(losdays ~ dow + age + unit.code, 
                  family = pospoisson(), 
                  data = data[i, ])
        
        as.vector(t(coef(summary(m))[, 1:2])) # %>% print
        
        
    }


# fn test: ---------------
set.seed(10)
bootresults <-
    boot(df1.raw.data,
         extract.coeffs.m4_function,
         R = 5) #  %>%

# Error in t.star[r, ] <- res[[r]] : 
    # number of items to replace is not a multiple of replacement length

# apparently some factor levels are dropped in some bootstrap iterations, 
# sooo... that's a problem
