myfunction <- function(t_input,n_rating,n_users,n_items,t_output) {

data <- read.table(file = t_input)

#use first 2 columns as indices and their input is the rating

ratings = data[,c(3)]
users = data[,c(1)]
items = data[,c(2)]


#for matrix[users[i]][items[i]] = ratings[i]

mat <- matrix(, nrow = n_users, ncol = n_items)


for (i in 1:n_rating)
    	{mat[users[i],items[i]]  = ratings [i] 
       }

#store average of ratings per each user
r_av <- vector()
for (i in 1:n_users){
r_av[i] <- mean(mat[i,], trim=0, na.rm = TRUE)}


#similarity function between 2 users u and v

  getSim <- function(u,v) 
  {count = 0
   items_set <- vector()
   v1 <- vector()
   v2 <- vector()

     for (i in 1:n_items)
       {
	    if (!is.na(mat[u,i]) && !is.na(mat[v,i]))
		{count = count + 1
		 items_set[count] = i}
        }
       #############################Replacing Na with 0##############################
       if (length(items_set)==0)
       {b <- 0
        return (b)} 
      
      #c=0
	#for (i in items_set){
	 for (i in 1:count){
	 v1[i] = mat[u,items_set[i]] - r_av[u]
       v2[i]= mat [v,items_set[i]] - r_av[v]
        }

        ############################Replacing Na with 0#############################
        if ( sum(v1*v1)==0 || sum(v2*v2)==0)
		#if ( v1==0 || v2==0)
       {b <- 0 
        return (b)}
    
       sim <- sum(v1*v2) / (sqrt(sum(v1*v1)) * sqrt(sum(v2*v2)))
      return (sim)
    
  }

sim_mat <- matrix(, nrow = n_users, ncol = n_users)

#Now get similarties between all users
for (i in 1:n_users)
{
	for ( j in 1:n_users)
       { 
	 sim_mat[i,j] = getSim(i,j)
	 }
}


#Computing the missing ratings for the unrated items for each user
predictR <- function(u,i){
sim_vec <- vector()
count = 0
r_v <- vector()
for (v in 1:n_users)
{ 
 
  if ((v!= u) && (!is.na(mat[v,i]))){ #only users who rated this item

  count = count + 1
  sim_vec[count] = (getSim(u,v))
   r_v[count] = mat[v,i] - r_av[v]

   }   
} 

# return r[av] which is the average of ratings per user if the other term is Na or 0
val=((sum(sim_vec*r_v))/(sum(abs(sim_vec))))
#if (is.na((sum(abs(sim_vec)))))
if(is.na(val))

p=r_av[u]
else
p = r_av[u] + val
return (p)

}

for (u in 1:n_users){
	for (i in 1:n_items){
		if (is.na(mat[u,i])){
			if (predictR(u,i)>5)
			mat[u,i] = 5
			else
			mat[u,i]=predictR(u,i)
		
		}
	}
}

out <- matrix(, nrow = n_users*n_items, ncol = 3)




write.csv(mat,"output.txt")
write.table(mat,"output_table.txt")
print(mat)


}