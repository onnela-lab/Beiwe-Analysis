log.lik.Psi_z = function(psi_hat, z, tau){
  lik = dnorm(z,psi_hat[7],psi_hat[8])-
       (dnorm(z,psi_hat[7],psi_hat[8])-dnorm(z,psi_hat[5],psi_hat[6]))*
       (pnorm(tau,psi_hat[1],psi_hat[2])-pnorm(tau,psi_hat[3],psi_hat[4]))
  ifelse(lik>0, log(lik), NA)
}

sum_log.lik.Psi_t_and_z = function(psi_hat, z, tau, TsTa) # These functions are very similar to the log likelihood for T|Z,Psi, and are used to find the MLE for T|Psi rather than score equations.
  log(dnorm(TsTa[1],psi_hat[1],psi_hat[2])) + log(dnorm(TsTa[2],psi_hat[3],psi_hat[4])) + sum(diff(c(tau,
  )) * log(ifelse(TsTa[1]<tau & tau<TsTa[2], dnorm(z,psi_hat[5],psi_hat[6]), dnorm(z,psi_hat[7],psi_hat[8]))))
sum_inv.lik.Psi_t_z = function(psi, z, tau, TsTa)
  sum(diff(c(tau,24)) / (ifelse(TsTa[1]<tau & tau<TsTa[2], dnorm(z,psi_hat[5],psi_hat[6]), dnorm(z,psi_hat[7],psi_hat[8]))))

lik.z_Psi = function(psi_hat,z,tau,mesh=0.25){
  SUM = 0
  for(i in seq(0,24,by=mesh)){
    for(j in seq(0,24,by=mesh)){
      SUM = SUM + exp(sum_log.lik.Psi_t_and_z(psi_hat,z,tau,c(i,j)))*mesh^2
    }
  }
  return(SUM)
}

log.lik.Psi_t_z = function(psi_hat, z, tau, TsTa, log.P.Z)
  log(dnorm(TsTa[1],psi_hat[1],psi_hat[2])) + log(dnorm(TsTa[2],psi_hat[3],psi_hat[4])) + sum(diff(c(tau,24)) * log(ifelse(TsTa[1]<tau & tau<TsTa[2], dnorm(z,psi_hat[5],psi_hat[6]), dnorm(z,psi_hat[7],psi_hat[8])))) - log.P.Z

######################   Score Equations   ######################

### T given Z, Psi ###
score_T.s = function(TsTa, z, tau, psi_hat)
  (dnorm(TsTa[2],psi_hat[7],psi_hat[8])-dnorm(TsTa[1],psi_hat[5],psi_hat[6])) * sum_inv.lik.Psi_t_z(psi_hat, z, tau,TsTa) - (TsTa[1]-psi_hat[1])/psi_hat[2]^2  
score_T.a = function(TsTa, z, tau, psi_hat)
  (dnorm(TsTa[1],psi_hat[5],psi_hat[6])-dnorm(TsTa[2],psi_hat[7],psi_hat[8])) * sum_inv.lik.Psi_t_z(psi_hat, z, tau,TsTa) - (TsTa[2]-psi_hat[3])/psi_hat[4]^2


### Z given Psi ###

# partial derivatives
d.phi_d.mu = function(z,mu,sigma)
    dnorm(z,mu,sigma)*(z-mu)/sigma^2
d.phi_d.sigma = function(z,mu,sigma)
  dnorm(z,mu,sigma)*((z-mu)^2/sigma^3-1/sigma)
d.Phi_d.mu = function(z,mu,sigma)
  -dnorm(z,mu,sigma)
d.Phi_d.sigma = function(z,mu,sigma)
  -dnorm(z,mu,sigma)*(z-mu)/sigma

score_mu_T.s    = function(psi_hat, z, tau)
  d.Phi_d.mu   (tau,psi_hat[1],psi_hat[2])/exp(log.lik.Psi_z(psi_hat, z, tau)) *  (dnorm(z,   psi_hat[5], psi_hat[6])-dnorm(z,   psi_hat[7], psi_hat[8]))
score_sigma_T.s = function(psi_hat, z, tau)
  d.Phi_d.sigma(tau,psi_hat[1],psi_hat[2])/exp(log.lik.Psi_z(psi_hat, z, tau)) *  (dnorm(z,   psi_hat[5], psi_hat[6])-dnorm(z,   psi_hat[7], psi_hat[8]))
score_mu_T.a    = function(psi_hat, z, tau)
  d.Phi_d.mu   (tau,psi_hat[3],psi_hat[4])/exp(log.lik.Psi_z(psi_hat, z, tau)) *  (dnorm(z,   psi_hat[7], psi_hat[8])-dnorm(z,   psi_hat[5], psi_hat[6]))
score_sigma_T.a = function(psi_hat, z, tau)
  d.Phi_d.sigma(tau,psi_hat[3],psi_hat[4])/exp(log.lik.Psi_z(psi_hat, z, tau)) *  (dnorm(z,   psi_hat[7], psi_hat[8])-dnorm(z,   psi_hat[5], psi_hat[6]))
score_mu_Z.s    = function(psi_hat, z, tau)
  d.phi_d.mu   (z,psi_hat[5],psi_hat[6])  /exp(log.lik.Psi_z(psi_hat, z, tau)) *  (pnorm(tau, psi_hat[1], psi_hat[2])-pnorm(tau, psi_hat[3], psi_hat[4]))
score_sigma_Z.s = function(psi_hat, z, tau)
  d.phi_d.sigma(z,psi_hat[5],psi_hat[6])  /exp(log.lik.Psi_z(psi_hat, z, tau)) *  (pnorm(tau, psi_hat[1], psi_hat[2])-pnorm(tau, psi_hat[3], psi_hat[4]))
score_mu_Z.a    = function(psi_hat, z, tau)
  d.phi_d.mu   (z,psi_hat[7],psi_hat[8])  /exp(log.lik.Psi_z(psi_hat, z, tau)) *(1+pnorm(tau, psi_hat[3], psi_hat[4])-pnorm(tau, psi_hat[1], psi_hat[2]))
score_sigma_Z.a = function(psi_hat, z, tau)
  d.phi_d.sigma(z,psi_hat[7],psi_hat[8])  /exp(log.lik.Psi_z(psi_hat, z, tau)) *(1+pnorm(tau, psi_hat[3], psi_hat[4])-pnorm(tau, psi_hat[1], psi_hat[2]))

scores = function(psi_hat, zs, taus, diff_taus){
  output = matrix(NA, 8, 1)
  output[1,] = sum(score_mu_T.s   (psi_hat, zs, taus)*diff_taus)
  output[2,] = sum(score_sigma_T.s(psi_hat, zs, taus)*diff_taus)
  output[3,] = sum(score_mu_T.a   (psi_hat, zs, taus)*diff_taus)
  output[4,] = sum(score_sigma_T.a(psi_hat, zs, taus)*diff_taus)
  output[5,] = sum(score_mu_Z.s   (psi_hat, zs, taus)*diff_taus)
  output[6,] = sum(score_sigma_Z.s(psi_hat, zs, taus)*diff_taus)
  output[7,] = sum(score_mu_Z.a   (psi_hat, zs, taus)*diff_taus)
  output[8,] = sum(score_sigma_Z.a(psi_hat, zs, taus)*diff_taus)
  return(output)
}

functions = list(score_mu_T.s,score_sigma_T.s, score_mu_T.a,score_sigma_T.a, score_mu_Z.s,score_sigma_Z.s, score_mu_Z.a,score_sigma_Z.a)

scores_list = function(data, psi_hat){
  functions %>% lapply(function(func){
    names(data) %>% lapply(function(dayname){sum(func(psi_hat,
          unlist(data[[dayname]]["Objective"]),
          unlist(data[[dayname]]["Hour_In_Day"]))*
          unlist(data[[dayname]]["diff_Hour_In_Day"]),na.rm=T)}) %>%
      unlist() %>% sum(na.rm=T)
  }) %>% unlist()
}

###################   Empirical Information   ###################

empirical_information_psi = function(psi_hat, zs, taus, diff_taus){ # consider making like the list version, for parsimony.
  output = matrix(NA,8,8)
  output[1,1] = sum(score_mu_T.s   (psi_hat,zs,taus)*score_mu_T.s   (psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[1,2] = sum(score_mu_T.s   (psi_hat,zs,taus)*score_sigma_T.s(psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[1,3] = sum(score_mu_T.s   (psi_hat,zs,taus)*score_mu_T.a   (psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[1,4] = sum(score_mu_T.s   (psi_hat,zs,taus)*score_sigma_T.a(psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[1,5] = sum(score_mu_T.s   (psi_hat,zs,taus)*score_mu_Z.s   (psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[1,6] = sum(score_mu_T.s   (psi_hat,zs,taus)*score_sigma_Z.s(psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[1,7] = sum(score_mu_T.s   (psi_hat,zs,taus)*score_mu_Z.a   (psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[1,8] = sum(score_mu_T.s   (psi_hat,zs,taus)*score_sigma_Z.a(psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  
  output[2,1] = sum(score_sigma_T.s(psi_hat,zs,taus)*score_mu_T.s   (psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[2,2] = sum(score_sigma_T.s(psi_hat,zs,taus)*score_sigma_T.s(psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[2,3] = sum(score_sigma_T.s(psi_hat,zs,taus)*score_mu_T.a   (psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[2,4] = sum(score_sigma_T.s(psi_hat,zs,taus)*score_sigma_T.a(psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[2,5] = sum(score_sigma_T.s(psi_hat,zs,taus)*score_mu_Z.s   (psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[2,6] = sum(score_sigma_T.s(psi_hat,zs,taus)*score_sigma_Z.s(psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[2,7] = sum(score_sigma_T.s(psi_hat,zs,taus)*score_mu_Z.a   (psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[2,8] = sum(score_sigma_T.s(psi_hat,zs,taus)*score_sigma_Z.a(psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  
  output[3,1] = sum(score_mu_T.a   (psi_hat,zs,taus)*score_mu_T.s   (psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[3,2] = sum(score_mu_T.a   (psi_hat,zs,taus)*score_sigma_T.s(psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[3,3] = sum(score_mu_T.a   (psi_hat,zs,taus)*score_mu_T.a   (psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[3,4] = sum(score_mu_T.a   (psi_hat,zs,taus)*score_sigma_T.a(psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[3,5] = sum(score_mu_T.a   (psi_hat,zs,taus)*score_mu_Z.s   (psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[3,6] = sum(score_mu_T.a   (psi_hat,zs,taus)*score_sigma_Z.s(psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[3,7] = sum(score_mu_T.a   (psi_hat,zs,taus)*score_mu_Z.a   (psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[3,8] = sum(score_mu_T.a   (psi_hat,zs,taus)*score_sigma_Z.a(psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  
  output[4,1] = sum(score_sigma_T.a(psi_hat,zs,taus)*score_mu_T.s   (psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[4,2] = sum(score_sigma_T.a(psi_hat,zs,taus)*score_sigma_T.s(psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[4,3] = sum(score_sigma_T.a(psi_hat,zs,taus)*score_mu_T.a   (psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[4,4] = sum(score_sigma_T.a(psi_hat,zs,taus)*score_sigma_T.a(psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[4,5] = sum(score_sigma_T.a(psi_hat,zs,taus)*score_mu_Z.s   (psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[4,6] = sum(score_sigma_T.a(psi_hat,zs,taus)*score_sigma_Z.s(psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[4,7] = sum(score_sigma_T.a(psi_hat,zs,taus)*score_mu_Z.a   (psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[4,8] = sum(score_sigma_T.a(psi_hat,zs,taus)*score_sigma_Z.a(psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  
  output[5,1] = sum(score_mu_Z.s   (psi_hat,zs,taus)*score_mu_T.s   (psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[5,2] = sum(score_mu_Z.s   (psi_hat,zs,taus)*score_sigma_T.s(psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[5,3] = sum(score_mu_Z.s   (psi_hat,zs,taus)*score_mu_T.a   (psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[5,4] = sum(score_mu_Z.s   (psi_hat,zs,taus)*score_sigma_T.a(psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[5,5] = sum(score_mu_Z.s   (psi_hat,zs,taus)*score_mu_Z.s   (psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[5,6] = sum(score_mu_Z.s   (psi_hat,zs,taus)*score_sigma_Z.s(psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[5,7] = sum(score_mu_Z.s   (psi_hat,zs,taus)*score_mu_Z.a   (psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[5,8] = sum(score_mu_Z.s   (psi_hat,zs,taus)*score_sigma_Z.a(psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  
  output[6,1] = sum(score_sigma_Z.s(psi_hat,zs,taus)*score_mu_T.s   (psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[6,2] = sum(score_sigma_Z.s(psi_hat,zs,taus)*score_sigma_T.s(psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[6,3] = sum(score_sigma_Z.s(psi_hat,zs,taus)*score_mu_T.a   (psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[6,4] = sum(score_sigma_Z.s(psi_hat,zs,taus)*score_sigma_T.a(psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[6,5] = sum(score_sigma_Z.s(psi_hat,zs,taus)*score_mu_Z.s   (psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[6,6] = sum(score_sigma_Z.s(psi_hat,zs,taus)*score_sigma_Z.s(psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[6,7] = sum(score_sigma_Z.s(psi_hat,zs,taus)*score_mu_Z.a   (psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[6,8] = sum(score_sigma_Z.s(psi_hat,zs,taus)*score_sigma_Z.a(psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  
  output[7,1] = sum(score_mu_Z.a   (psi_hat,zs,taus)*score_mu_T.s   (psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[7,2] = sum(score_mu_Z.a   (psi_hat,zs,taus)*score_sigma_T.s(psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[7,3] = sum(score_mu_Z.a   (psi_hat,zs,taus)*score_mu_T.a   (psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[7,4] = sum(score_mu_Z.a   (psi_hat,zs,taus)*score_sigma_T.a(psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[7,5] = sum(score_mu_Z.a   (psi_hat,zs,taus)*score_mu_Z.s   (psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[7,6] = sum(score_mu_Z.a   (psi_hat,zs,taus)*score_sigma_Z.s(psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[7,7] = sum(score_mu_Z.a   (psi_hat,zs,taus)*score_mu_Z.a   (psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[7,8] = sum(score_mu_Z.a   (psi_hat,zs,taus)*score_sigma_Z.a(psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  
  output[8,1] = sum(score_sigma_Z.a(psi_hat,zs,taus)*score_mu_T.s   (psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[8,2] = sum(score_sigma_Z.a(psi_hat,zs,taus)*score_sigma_T.s(psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[8,3] = sum(score_sigma_Z.a(psi_hat,zs,taus)*score_mu_T.a   (psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[8,4] = sum(score_sigma_Z.a(psi_hat,zs,taus)*score_sigma_T.a(psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[8,5] = sum(score_sigma_Z.a(psi_hat,zs,taus)*score_mu_Z.s   (psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[8,6] = sum(score_sigma_Z.a(psi_hat,zs,taus)*score_sigma_Z.s(psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[8,7] = sum(score_sigma_Z.a(psi_hat,zs,taus)*score_mu_Z.a   (psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  output[8,8] = sum(score_sigma_Z.a(psi_hat,zs,taus)*score_sigma_Z.a(psi_hat,zs,taus)*exp(log.lik.Psi_z(psi_hat,zs,taus))*diff_taus,na.rm=T)
  
  return(output)
}

empirical_information_psi_list = function(data, psi_hat){
  function_pair_indeces = expand.grid(1:8,1:8)
  function_pair_indeces %>% apply(1,function(ind){
    names(data) %>% lapply(function(dayname){
      temp_Z = unlist(data[[dayname]]["Objective"])
      temp_taus = unlist(data[[dayname]]["Hour_In_Day"])
      temp_diff_taus = unlist(data[[dayname]]["diff_Hour_In_Day"])
      to_sum = functions[[ind[1]]](psi_hat,temp_Z,temp_taus)*
        functions[[ind[2]]](psi_hat,temp_Z,temp_taus)*
        exp(log.lik.Psi_z(psi_hat,temp_Z,temp_taus))*temp_diff_taus
      sum(to_sum,na.rm=T)
    }) %>% unlist %>% sum(na.rm=T)
  }) %>% matrix(8,8)
}

outputs = c()
empirical_information_TsTa = function(psi, z, tau, mesh = 0.25){
  output = matrix(0,2,2)
  log.P.Z = log(lik.z_Psi(psi, z, tau))
  for(i in seq(0,24,by=mesh)){
    for(j in seq(0,24,by=mesh)){
      output[1,1] = output[1,1] + score_T.s(c(i,j),z,tau,psi)*score_T.s(c(i,j),z,tau,psi)*exp(log.lik.Psi_t_z(psi,z,tau,c(i,j),log.P.Z))*mesh^2
      output[1,2] = output[1,2] + score_T.s(c(i,j),z,tau,psi)*score_T.a(c(i,j),z,tau,psi)*exp(log.lik.Psi_t_z(psi,z,tau,c(i,j),log.P.Z))*mesh^2
      output[2,1] = output[2,1] + score_T.a(c(i,j),z,tau,psi)*score_T.s(c(i,j),z,tau,psi)*exp(log.lik.Psi_t_z(psi,z,tau,c(i,j),log.P.Z))*mesh^2
      output[2,2] = output[2,2] + score_T.a(c(i,j),z,tau,psi)*score_T.a(c(i,j),z,tau,psi)*exp(log.lik.Psi_t_z(psi,z,tau,c(i,j),log.P.Z))*mesh^2
      outputs = c(outputs, score_T.s(c(i,j),z,tau,psi)*score_T.s(c(i,j),z,tau,psi)*exp(log.lik.Psi_t_z(psi,z,tau,c(i,j),log.P.Z))*mesh^2)
    }
  }
  return(output)
}

######################   Solving: Newton Raphson and Brute Force   ######################

# Newton_Raphson
Newton_Raphson = function(psi_0, zs, taus, diff_taus, minimum_change_tolerated = .02, max_delta = .2, min_steps = 30, max_steps=200,record=FALSE, variance_minimum = NA, variance_maximum=6){
  D = nrow(taus)
  old_psi_hat = rep(0,8)
  psi_hat = psi_0
  if(record) psi_hats = psi_hat
  Q = 0
  while((abs(max(old_psi_hat-psi_hat))> minimum_change_tolerated | Q < min_steps) & Q<max_steps){
    Q = Q + 1
    old_psi_hat = psi_hat
    delta = solve(empirical_information_psi(psi_hat, zs, taus, diff_taus)) %*% scores(psi_hat,zs, taus, diff_taus) 
    steps = -psi_hat/2/delta
    step_size = min(c(steps[which(steps>=0)]),na.rm=T) # taking this minimum ensures computed changes are always valid.
    delta = delta * step_size
    if(max(abs(delta)) > max_delta)
      delta = delta * max_delta  / max(abs(delta)) 
    psi_hat   = psi_hat + delta
    if(!is.na(variance_minimum)){
      for(j in c(2,4,6,8))
        psi_hat[j] = max(psi_hat[j], variance_minimum)
      for(j in c(2,4))
        psi_hat[j] = min(psi_hat[j], variance_maximum)
    }
    if(record) psi_hats = cbind(psi_hats,psi_hat)
    print(Q)
    print(psi_hat)
  }
  SD = sqrt(diag(solve(empirical_information_psi(psi_hat, Z, times, diff_times))))
  output = list("psi_hat"=psi_hat,"SD"=SD, "num_steps"=Q+1)
  if(record)
    output$psi_hats=psi_hats
  return(output)
}

Newton_Raphson_list = function(data, psi_0, minimum_change_tolerated = .02, max_delta = .2, min_steps = 30, max_steps=200, temperature = 0.05, record=FALSE, variance_minimum = NA, variance_maximum=6){
  D = length(data)
  old_psi_hat = rep(0,8)
  psi_hat = psi_0
  if(record) psi_hats = psi_hat
  Q = 0
  while((abs(max(old_psi_hat-psi_hat))> minimum_change_tolerated | Q < min_steps) & Q<max_steps){
    Q = Q + 1
    old_psi_hat = psi_hat
    delta = temperature * solve(empirical_information_psi_list(data, psi_hat)) %*% scores_list(data, psi_hat) 
    #steps = -psi_hat/2/delta
    #step_size = min(c(steps[which(steps>=0)]),na.rm=T) # taking this minimum ensures computed changes are always valid.
    #delta = delta * step_size
    if(max(abs(delta)) > max_delta)
      delta = delta * max_delta  / max(abs(delta)) 
    psi_hat   = psi_hat + delta
    if(!is.na(variance_minimum)){
      for(j in c(2,4,6,8))
        psi_hat[j] = max(psi_hat[j], variance_minimum)
      for(j in c(2,4))
        psi_hat[j] = min(psi_hat[j], variance_maximum)
    }
    if(record) psi_hats = cbind(psi_hats,psi_hat)
    print(Q)
    print(psi_hat)
  }
  SD = sqrt(diag(solve(empirical_information_psi_list(data, psi_hat))))
  output = list("psi_hat"=psi_hat,"SD"=SD, "num_steps"=Q+1)
  if(record)
    output$psi_hats=psi_hats
  return(output)
}

# brute-force optimization
max_lik_Psi_t_z = function(psi, z, tau,mesh=0.25){
  MAX = c(-Inf,0,0)
  for(k in seq(1,24,by=mesh)){
    for(l in seq(k,24,by=mesh)){
      lik = sum_log.lik.Psi_t_and_z(psi, z, tau,c(k,l))
      if(!is.na(lik)){
      if(lik > MAX[1]){
        MAX = c(lik,k,l)
      }
      }
    }
  }
  return(MAX)
}
