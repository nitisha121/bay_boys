births <- read.csv("C:/My data/university/third year/stat331/final_proj/male_births/chds_births.csv")

# load mice
library(mice)
library(styler)

imp_all <- mice(births)

## STEPWISE MODELS
# i =5
Ms5 <-lm(formula = wt ~ gestation + parity + mht + mwt + feth + marital + 
     income + smoke + time + number + gestation:income + gestation:number + 
     parity:marital + mht:marital + mwt:income + time:number + 
       gestation:mwt + gestation:parity + parity:mht, data = complete(imp_all,5))




# i =4
Ms4 <- lm(formula = wt ~ gestation + parity + mht + mwt + feth + income + 
            smoke + time + number + gestation:income + gestation:number + 
            mwt:income + parity:mht + gestation:mwt, data = complete(imp_all,4))




# i =3
Ms3 <- lm(formula = wt ~ gestation + parity + mage + mht + mwt + feth + 
            marital + smoke + time + number + gestation:number + gestation:mage + 
            parity:marital + mht:marital + gestation:mwt + gestation:mht + 
            parity:mwt + mage:mwt + time:number, data = complete(imp_all,3))




# i=2
Ms2 <- lm(formula = wt ~ gestation + parity + mage + mht + mwt + feth + 
            income + smoke + time + number + gestation:income + gestation:number + 
            gestation:mage + parity:mht + gestation:mwt + time:number + 
            mwt:income + mht:income + mage:mht, data = complete(imp_all,2))



# i =1
Ms1 <- lm(formula = wt ~ gestation + parity + mht + mwt + feth + marital + 
            income + smoke + time + number + gestation:income + gestation:number + 
            parity:marital + mht:marital + mwt:income + parity:mht + 
            time:number + gestation:mwt + gestation:parity, data = complete(imp_all,1))



## STEPWISE SUMMARY TABLE
mtable.step <- mtable("Model 1" = Ms1, "Model 2" = Ms2, "Model 3" = Ms3,
                      "Model 4" = Ms4, "Model 5" = Ms5, summary.stats=c("Adj-R-squared", "AIC"))
mtable.step

## BACKWARD MODEL SELECTION
Mb1 <- lm(formula = wt ~ gestation + parity + mht + mwt + fage + marital + 
            income + smoke + time + number + feth + gestation:mht + gestation:mwt + 
            gestation:fage + gestation:income + gestation:smoke + gestation:time + 
            gestation:number + parity:mht + parity:marital + mht:fage + 
            mht:marital + mht:income + mwt:income + fage:smoke + fage:number, 
          data = complete(imp_all,1))

Mb2 <- lm(formula = wt ~ gestation + parity + mht + mwt + fage + marital + 
            income + smoke + time + number + feth + gestation:mht + gestation:mwt + 
            gestation:fage + gestation:income + gestation:number + parity:mht + 
            parity:marital + mht:fage + mht:marital + mht:income + mwt:income + 
            fage:smoke + fage:number + income:number + time:number, data = complete(imp_all,2))

Mb3 <- lm(formula = wt ~ gestation + parity + mage + mht + mwt + fage + 
            marital + income + smoke + time + number + feth + gestation:mage + 
            gestation:mht + gestation:mwt + gestation:income + gestation:smoke + 
            gestation:time + gestation:number + parity:mht + parity:fage + 
            parity:marital + parity:income + mage:income + mht:fage + 
            mht:marital + mht:income + mwt:income + fage:smoke + fage:number + 
            income:number + time:number, data = complete(imp_all,3))

Mb4 <- lm(formula = wt ~ gestation + parity + mage + mht + mwt + fage + 
            marital + income + smoke + time + number + feth + gestation:mht + 
            gestation:mwt + gestation:fage + gestation:income + gestation:number + 
            parity:mht + parity:marital + mage:mht + mht:marital + mht:income + 
            mwt:income + fage:smoke + fage:number + time:number, data = complete(imp_all,4))
Mb5 <- lm(formula = wt ~ gestation + parity + mage + mht + mwt + fage + 
            marital + income + smoke + time + number + feth + gestation:mht + 
            gestation:mwt + gestation:fage + gestation:income + gestation:smoke + 
            gestation:time + gestation:number + parity:mwt + parity:marital + 
            parity:smoke + parity:time + mage:mwt + mage:fage + mht:marital + 
            mht:income + mwt:income + fage:smoke + fage:number + smoke:number + 
            time:number, data = complete(imp_all,5))


mtable.back <- mtable("Model 1" = Mb1, "Model 2" = Mb2, "Model 3" = Mb3,
                      "Model 4" = Mb4, "Model 5" = Mb5, summary.stats=c("R-squared", "AIC"))
mtable.back
  
