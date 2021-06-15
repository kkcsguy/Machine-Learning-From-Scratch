dat <- as.data.frame(dat)
dat$assignments <- assignments
# write your code here
distances <- function(point, means){
  # write code here
  n <- nrow(point)
  dist_to_centroid_A <- rep(NA,n)
  dist_to_centroid_B <- rep(NA,n)
  dist_to_centroid_C <- rep(NA,n)
  for( i in 1:n){
    dist_to_centroid_A[i] <- sqrt((point[i,1] - means[1,2])^2 + (point[i,2] - means[1,3])^2)
    dist_to_centroid_B[i] <- sqrt((point[i,1] - means[2,2])^2 + (point[i,2] - means[2,3])^2)
    dist_to_centroid_C[i] <- sqrt((point[i,1] - means[3,2])^2 + (point[i,2] - means[3,3])^2)
  }
  return(cbind(dist_to_centroid_A,dist_to_centroid_B,dist_to_centroid_C))
}

converged = FALSE
while(!converged){
  # write code here...
  means <- dat %>% group_by(assignments) %>% summarise(x1=mean(V1),x2=mean(V2))
  dist <- distances(dat,means)
  assignments_new <- apply(dist,1,which.min)
  if(all(assignments_new == dat$assignments)){
    converged = TRUE
  }else{
    dat$assignments <- assignments_new
    plot(dat[,1:2], col = assignments_new, asp = 1)
    points(centroids$x1, centroids$x2, col = centroids$assignments, cex = 2, 
           pch =19)
  }
}

table(assignments_new,true_groups)

-------------------------------------------------------------
distances <- function(point, means){
  dist_to_centroid_A <- sqrt(sum((point - means[1,c(2,3)]) ^ 2))
  dist_to_centroid_B <- sqrt(sum((point - means[2,c(2,3)]) ^ 2)) 
  dist_to_centroid_C <- sqrt(sum((point - means[3,c(2,3)]) ^ 2)) 
  return(c(A = dist_to_centroid_A, B = dist_to_centroid_B, C =dist_to_centroid_C))
}
converged = FALSE
while(!converged){
  data <- data.frame(dat, assignments)
  old_assignment <- assignments
  centroids <- data %>% group_by(assignments) %>% summarise(x1 = mean(X1), 
                                                            x2 =mean(X2))
  dist <- matrix(NA,nrow= nrow(dat),ncol = 3)
  for(i in 1:nrow(dat)){ 
    dist[i,]<-distances(dat[i,],centroids) 
    assignments[i] <- which.min(dist[i,])
  }
  plot(dat, col = assignments, asp = 1)
  points(centroids$x1, centroids$x2, col = centroids$assignments, cex = 2, 
         pch =19)
  if(all(old_assignment==assignments)){ 
    converged <- TRUE
  }
}
table(assignments,true_groups)



---------------------------------------------------------------

distances <- function(point, means){
    outputs <- matrix(NA, nrow=nrow(point), ncol=nrow(means))
    for (i in 1:nrow(means)) {
      centroid <- means[i,]
      outputs[,i] <- apply((dat-rep(1, nrow(point))%o%centroid)^2, 1, sum)
    }
    return(outputs)
  }

centroids <- function(point) {
  clusters <- levels(assignments)
  outputs <- matrix(NA, nrow=k, ncol=ncol(point))
  for (i in 1:length(clusters)) {
    cluster <- clusters[i]
    outputs[i,] <- apply(point[assignments == cluster, ], 2, mean)
  }
  return(outputs)
}

d <- distances(dat, centroids(dat))

converged = FALSE
i = 0
while(!converged){
  plot(dat, col = assignments, asp = 1)
  d <- distances(dat, centroids(dat))
  prev.assignments <- assignments
  assignments <- factor(apply(d, 1, which.min))
  i <- i + 1
  if (all(prev.assignments == assignments)) {
    converged = TRUE
  }
}

plot(dat, col = assignments, asp = 1)
assignments <- factor(as.character(assignments))
table(assignments, true_groups)


--------------------------------------------------

distances <- function(point, means,i){
    
    dist_to_centroid <- sqrt(((point-means[i,])[1])^2+((point-means[i,])[2])^2)
    
    return(dist_to_centroid)
  }

n <-dim(dat)[1]

converged = FALSE
rept <-1
while(!converged){
  par(mfrow = c(1,2))
  
  dat_df <- data.frame(dat, assignments)
  
  #create centroids
  centroids <- dat_df%>%group_by(assignments) %>%
    summarise(x1 = mean(X1), x2 = mean(X2))
  centroids
  plot(dat_df[,1:2], col = assignments, asp =1, pch = 20)
  points(centroids$x1,centroids$x2, col = centroids$assignments, cex = 2, pch = 13)
  
  
  #find distance among all pts to 3 centroids  
  cen_dis1<- matrix(0,nrow = n, ncol = 1)
  cen_dis2<- matrix(0,nrow = n, ncol = 1)
  cen_dis3<- matrix(0,nrow = n, ncol = 1)
  
  for (i in 1:n){
    cen_dis1[i]<-distances(dat_df[i,1:2],centroids[,2:3],1)
    cen_dis2[i]<-distances(dat_df[i,1:2],centroids[,2:3],2)
    cen_dis3[i]<-distances(dat_df[i,1:2],centroids[,2:3],3)
  }
  
  cen_dis<- cbind(cen_dis1,cen_dis2,cen_dis3)
  new_assignments <- apply(cen_dis,1,which.min)
  #plot  
  plot(dat_df[,1:2], col = new_assignments, asp =1, pch = 20)
  points(centroids$x1,centroids$x2, col = centroids$assignments, cex = 2, pch = 13)
  
  #decide converge true or false  
  if(all(assignments == new_assignments)){
    converged <- TRUE
    print(rept)
  }
  else{
    assignments<- new_assignments
    rept<-rept+1
  }
}