gamlss.cluster <- function(dframe,
                           method = 'all', 
                             n_cl = 2,
                           family = NO,
                      explanatory = TRUE,
                           ...){

  library(gamlss)
  library(cluster)
  library(mclust)
    
  if(class(dframe) != "data.frame") stop('Argument dframe must be a data frame')

  fami <<- family
      p <- NULL

  
  if(method == 'all'){
    amended.dat <<- data.frame(dframe, 
                               group_kmeans = as.factor(kmeans(dframe,n_cl)$cluster),
                               group_ward = as.factor(cutree(hclust(dist(dframe, method = "euclidean"), method="ward.D2"),n_cl)),
                               group_fuzzy = as.factor(fanny(dframe, n_cl, metric = "euclidean", stand = FALSE)$cluster),
                               group_modelbased = as.factor(Mclust(dframe,n_cl,verbose=F)$classification))
    
    cat('The response variable was divided into', n_cl, 'clusters through', method, 'methods',
        '\n New latent variables named group[method] were generated\n', sep = ' ')
    
    names(amended.dat)[1] <<- 'y'
       
    m0 <- gamlss(y ~ 1, 
                 data = amended.dat, 
                 family = fami,
                 trace = F)
    
        cat('\nTesting all stepwise-based procedures using', eval(gamlss.family(fami))$family[2] ,'distribution and different clustering methods\n', sep=' ')
        
        if(explanatory == TRUE){
        
       mkmeans <- stepGAICAll.A(m0, 
                                scope = list(lower=~1, 
                                             upper=as.formula(paste("~", paste(names(amended.dat[-c(1,
                                                                                                    dim(amended.dat)-2,
                                                                                                    dim(amended.dat)-1,
                                                                                                    dim(amended.dat))]), collapse="+"), sep=""))), 
                                trace=F)
       
       mward <- stepGAICAll.A(m0, 
                              scope = list(lower=~1, 
                                           upper=as.formula(paste("~", paste(names(amended.dat[-c(1,
                                                                                                  dim(amended.dat)-3,
                                                                                                  dim(amended.dat)-1,
                                                                                                  dim(amended.dat))]), collapse="+"), sep=""))),
                              trace=F)
       
       mfuzzy <- stepGAICAll.A(m0, 
                               scope = list(lower=~1, 
                                            upper=as.formula(paste("~", paste(names(amended.dat[-c(1,
                                                                                                   dim(amended.dat)-3,
                                                                                                   dim(amended.dat)-2,
                                                                                                   dim(amended.dat))]), collapse="+"), sep=""))),
                               trace=F)
       
       mmodbas <- stepGAICAll.A(m0, 
                                scope = list(lower=~1, 
                                             upper=as.formula(paste("~", paste(names(amended.dat[-c(1,
                                                                                                    dim(amended.dat)-3,
                                                                                                    dim(amended.dat)-2,
                                                                                                    dim(amended.dat)-1)]), collapse="+"), sep=""))),
                                trace=F)
             
             aic_cl <- row.names(AIC(mkmeans, mward, mfuzzy, mmodbas))[1]
             
             if(aic_cl == 'mkmeans'){
               for(i in 1:n_cl){
                 p <- c(p,table(amended.dat$group_kmeans)[i]/sum(table(amended.dat$group_kmeans)))
               }
             } else 
               
               if(aic_cl == 'mward'){
               for(i in 1:n_cl){
                 p <- c(p,table(amended.dat$group_ward)[i]/sum(table(amended.dat$group_ward)))
               }
             } else 
               
               if(aic_cl == 'mfuzzy'){
                   for(i in 1:n_cl){
                     p <- c(p,table(amended.dat$group_fuzzy)[i]/sum(table(amended.dat$group_fuzzy)))
                   }
                 } else{
             for(i in 1:n_cl){
               p <- c(p,table(amended.dat$group_modelbased)[i]/sum(table(amended.dat$group_modelbased)))
             }
             }
        
             mFinal <- get(aic_cl)
                
        }
        
        else{
          
          mkmeans <- stepGAICAll.A(m0, scope = list(lower=~1, upper=~group_kmeans), trace=F)
        
            mward <- stepGAICAll.A(m0, scope = list(lower=~1, upper=~group_ward), trace=F)
        
           mfuzzy <- stepGAICAll.A(m0, scope = list(lower=~1, upper=~group_fuzzy), trace=F)
        
          mmodbas <- stepGAICAll.A(m0, scope = list(lower=~1, upper=~group_modelbased), trace=F)
        
          aic_cl <- row.names(AIC(mkmeans, mward, mfuzzy, mmodbas))[1]
          
          if(aic_cl == 'mkmeans'){
            for(i in 1:n_cl){
              p <- c(p,table(amended.dat$group_kmeans)[i]/sum(table(amended.dat$group_kmeans)))
            }
          } else 
            
            if(aic_cl == 'mward'){
              for(i in 1:n_cl){
                p <- c(p,table(amended.dat$group_ward)[i]/sum(table(amended.dat$group_ward)))
              }
            } else 
              
              if(aic_cl == 'mfuzzy'){
                for(i in 1:n_cl){
                  p <- c(p,table(amended.dat$group_fuzzy)[i]/sum(table(amended.dat$group_fuzzy)))
                }
              } else{
                for(i in 1:n_cl){
                  p <- c(p,table(amended.dat$group_modelbased)[i]/sum(table(amended.dat$group_modelbased)))
                }
              }
          
           mFinal <- get(aic_cl)
          
        }
       
  } else{ 
  
  if(method == 'kmeans'){
    amended.dat <<- data.frame(dframe, group = as.factor(kmeans(dframe,n_cl)$cluster))
  } else
    
    if(method == 'ward'){
      amended.dat <<- data.frame(dframe, group = as.factor(cutree(hclust(dist(dframe, method = "euclidean"), method="ward.D2"),n_cl)))
      } else
        
        if(method == 'fuzzy'){
          amended.dat <<- data.frame(dframe, group = as.factor(fanny(dframe, n_cl, metric = "euclidean", stand = FALSE)$cluster))
          } else
            
            if(method == 'mbased'){
              amended.dat <<- data.frame(dframe, group = as.factor(Mclust(dframe,n_cl,verbose=F)$classification))
            }
    else stop('Please choose among one of the following methods: all, kmeans, ward, fuzzy or mbased')
    
    for(i in 1:n_cl){
      p <- c(p,table(amended.dat$group)[i]/sum(table(amended.dat$group)))
    }
    
    cat('The response variable was divided into', n_cl, 'clusters through', method, 'method',
        '\n A new latent variable named group was generated\n', sep = ' ')
    
    names(amended.dat)[1] <<- 'y'
    
    m0 <- gamlss(y ~ 1, 
                 data = amended.dat, 
                 family = fami,
                 trace = F)
    
    cat('\nStepwise-based procedure using', eval(gamlss.family(fami))$family[2] ,'distribution\n', sep=' ')
    
    ifelse(explanatory == TRUE,
           mFinal <- stepGAICAll.A(m0, 
                                   scope = list(lower=~1, 
                                                upper=as.formula(paste("~", paste(names(amended.dat[-1]), collapse="+"), sep="")))),
           mFinal <- stepGAICAll.A(m0, scope = list(lower=~1, upper=~group))
    )
  
    }
  
  on.exit(rm(amended.dat, fami, envir=.GlobalEnv))
  
                                     out <- list()
                             
                         out$amended.dat <- amended.dat
                      out$cluster.method <- method
                      out$cluster.number <- n_cl
                                   out$p <- p
                         out$final.model <- mFinal
                                   
  return(out)
  
}
