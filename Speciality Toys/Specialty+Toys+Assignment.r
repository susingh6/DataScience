# All the R code use for assignment solution is given below


#Mean= 0 , Sd =1
Z=qnorm(0.975,0,1)
Z

# Create a function to generate the normal distribution

gen_normal <- function(mean,sd,lb=0,ub=1)
{
   range = FALSE
# +- 3.5 sigma are as the extremes
   x= seq(-3.5,3.5,0.01)*sd + mean
   start = head(x,1)
   end = tail(x,1) 
   hx =dnorm(x,mean,sd)
   plot(x, hx, type="n", xlab="Speciality Toys", 
        ylab="Probability Density",main="Demand",
        sub= paste('Mean=',mean, 'SD=',sd),axes=TRUE)
   lines(x, hx)
 # Remove commennts in case mean and sd markings are required
 #  abline(v=mean)
 #  abline(v=mean+sd,col = "lightgray")
 #  abline(v=mean-sd,col = "lightgray")
 #  abline(v=mean+(2*sd),col = "lightgray")
 #  abline(v=mean-(2*sd),col = "lightgray")
 #  abline(v=mean+(3*sd),col = "lightgray")
 #  abline(v=mean-(3*sd),col = "lightgray")

# To see if no boundaries are supplied
   if (lb== 0 & ub==1){
       lb=head(x,1)
       ub= tail(x,1)       
            
   }
   else if (lb==0 & ub!=1){
        lb=head(x,1)
        
   }
   else {
        range = TRUE
       
   }
 
# Calculate the stock in probability area
   area = pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
   
 
# If no specific range mentioned
   if (range==FALSE) {
       i= x>=lb & x<=ub
       j= x>=ub & x<=end
       polygon(c(lb,x[i],ub), c(0,hx[i],0), col=rgb(0,0,1,0.5))
       polygon(c(ub,x[j],end), c(0,hx[j],0), col=rgb(1,0,0,0.5))
       if (ub==end){
           mtext(paste("P(",lb,"< X <",ub,")=",
                       round(area, digits=3)),3)
       }
       else{
# Display both stock in and stock out probability at the top
           mtext(paste("P(",lb,"< X <",ub,")=",
                round(area, digits=3)),3,adj=0.1)
           mtext(paste("P(X >",ub,")=",1-
                (round(area, digits=3))),3,adj=0.9)
       }


   }
   else{
# A range is supplied. We have 3 polygons to color
        i=x>=start & x<=lb
        j=x>=lb & x<=ub
        k= x>=ub & x<=end
       polygon(c(start,x[i],lb), c(0,hx[i],0),col=rgb(1,0,0,0.5))
       polygon(c(lb,x[j],ub), c(0,hx[j],0),col=rgb(0,0,1,0.5) )
       polygon(c(ub,x[k],end), c(0,hx[k],0),col=rgb(1,0,0,0.5))
       mtext(paste("P(",lb,"< X <",ub,")=",round(area, digits=3)),3)
   }
   
   legend("topright",legend=c('Out','In'),lty=1, 
          col=c('red', 'blue'))
   
   
}

# To create different normal plots and get stock in/out probabilities

gen_normal (20000, 5102)

gen_normal (20000, 5102,lb=10000,ub=30000)

gen_normal(20000, 5102,ub=15000)

gen_normal (20000, 5102,ub=18000)

gen_normal (20000, 5102,ub=24000)

gen_normal (20000, 5102,ub=28000)

# Get the required demand ('X') values for final suggestion

qnorm(.025,20000,5102)

qnorm(.050,20000,5102)

qnorm(.075,20000,5102)

qnorm(.10,20000,5102) 


