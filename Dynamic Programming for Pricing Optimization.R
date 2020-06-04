pricing_function <- function(days_left, tickets_left, demand_level){ 

t=tickets_left
d=days_left
dl=demand_level

#possible values of "d" and "t"
dValues=seq(0,d)
tValues = seq(0,t)

dN=length(dValues)
tN=length(tValues)

#Value function matrix
V = matrix(NA,dN,tN)
rownames(V) = dValues # add rownames and colnames to the V matrix
colnames(V) = tValues

#Action matrix
U = V # copy V matrix with rownames and colnames, to U matrix


#walk backwards in time

for (d in (dValues)){
  
  for (t in (tValues)){
  #for each time value the loop through the possible values of Tickets
  #cat(d,t)
    if(d==0){
      # Boundary condition
      V[paste(d), paste(t)]=0
    }else if(t==0){
      # Boundart Condition 
      V[paste(d), paste(t)]=0
    }else
      {
      # Bellman Equation
        p = seq(0,dl) # checking for all values of prices less than demand level
        valueChoices=seq(0,dl)
        
        for (index in 1:length(p)){
          valueChoices[index]= p[index]*max(min(t,dl-p[index]),0) +V[paste(d-1),paste(t-max(min(t,dl-p[index]),0))]
          }
        #print(valueChoices)
        #cat(valueChoices,"----------------")
        V[paste(d),paste(t)]=max(valueChoices)
        U[paste(d),paste(t)]=p[which.max(valueChoices)]
    } #if
  } #for s
} #for t
return(U[d+1,t+1])
}
pricing_function(1,2,10)



