

#***************************************************
# FUNCTION TO FIT MODEL M3 FOR BOOTSTRAPPING 
#***************************************************

library("broom")

# function definition: ----------
extract.coeffs.m3_function <- 
    function(data, i){
        # data:  is a df
        # i:  is just an iterating index 
        
        require("magrittr")
        require("VGAM")
        require("boot")
        
        m <- vglm(losdays ~ dow, 
                  family = pospoisson(), 
                  data = data[i, ])
        
        as.vector(t(coef(summary(m))[, 1:2]))
        # coef(m3.los.vs.dow)[1] %>% as.vector
        
    }


# fn test: ---------------
# set.seed(10)
# bootresults <- 
#     boot(df1.raw.data, 
#          extract.coeffs.m3_function, 
#          R = 100) %>% 
#     tidy() %>% 
#     mutate(param = rep(c("coeff", "stderror"), 7)) %>% 
#     select(param, everything())
# 
# bootresults
