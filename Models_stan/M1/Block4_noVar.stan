data {
  int NS;//number of subjects
  int MT;//maximum number of trials
  int NStim;//number of stimuli (4)
  int NC;//number of choices (2)
  int NT[NS];//number of trials per subject
  int K;//number of coefficients for glm
  int stim[NS,MT];//stimulus shown (1-4)
  real<lower=-1,upper=1> rew[NS,MT];//subject x trial reward, -1 for missed
  int choice[NS,MT];//chosen option, -1 for missed
  int choice_two[NS,MT];//1=chose left,0=chose right, -1 for missed
}

transformed data{
  int N;
  N = 0;
  for (s in 1:NS) {
    for (t in 1:NT[s]) {
      if (choice[s,t] > 0) {
        N = N+1;
      }
    }
  }
}

parameters {
  //parameters for group alpha distribution
  real<lower=0,upper=1> alpha;
  
  //parameters for group beta distributions
  vector[K] b_mean;
  //vector<lower=0>[K] b_sd;
  
  //subject-level alpha and betas
  //real<lower=0,upper=1> alpha[NS];
  // matrix[NS,K] beta;
	
	//cholesky factorization of correlation matrix of subject-level estimates
  //cholesky_factor_corr[K] Lcorr;
}

transformed parameters{
  //subject x trials x choice Q value matrix
  real Q[NS,MT,NStim,NC];
  
  //prediction error matrix
  real delta[NS,MT];
  
//need to assign Q and PE because missing trials will recreate nan's otherwise
  for (s in 1:NS){
    for (m in 1:MT){
      for (st in 1:NStim){
        for (c in 1:NC){
          Q[s,m,st,c]=0.0;
        }
      }
    }
  }
  
  delta=rep_array(0.0,NS,MT);
  
  for (s in 1:NS) {
  	for (t in 1:NT[s]) {
  	  
  	  //set initial values of Q and delta on first trial
		  if(t == 1) {
		    for (st in 1:NStim){
		      for (c in 1:NC){
		        Q[s,t,st,c]=.5;//want to change to 1/NC if stan ever allows int_to_real
		      }
		    }
		    delta[s,t]=0;
		  }
		    if (choice[s,t] >= 0){
		      //PE = reward-expected
		      delta[s,t]=rew[s,t]-Q[s,t,stim[s,t],choice[s,t]];
		      
		      if (t<NT[s]){
		        //update value with alpha-weighted PE
		        for (st in 1:NStim){
		          if (stim[s,t]==st){
		            Q[s,t+1,st,choice[s,t]]= Q[s,t,st,choice[s,t]] +
		            alpha*delta[s,t];
		          }else{		        
		            //value of chosen option for unpresented stims not updated
		            Q[s,t+1,st,choice[s,t]]= Q[s,t,st,choice[s,t]];
		          }
		            //value of unchosen option is not updated (for any stimuli)
		            Q[s,t+1,st,abs(choice[s,t]-3)] = Q[s,t,st,abs(choice[s,t]-3)];
		      }
		    } else {
		        //if no response, keep Q value and set delta to 0
		        if (t<NT[s]){
		          for (st in 1:NStim){
		            for (c in 1:NC){
		              Q[s,t+1,st,c]=Q[s,t,st,c];
		            }
		          }
		        delta[s,t]=0;
		        }
		      }
      }
    }
  }
}


model {
  //hyperpriors
  alpha ~ beta(2,2);
  b_mean ~ normal(0,5);
  //b_sd ~ cauchy(0,5);
  
  //distributions of subject effects
  //alpha ~ beta(0.5,0.5);
  //beta[:,1] ~ normal(0,5);
  //beta[:,2] ~ normal(0,5);
   
  //for (s in 1:NS){
  //beta[s]~multi_normal_cholesky(b_mean,diag_pre_multiply(b_sd,Lcorr));
  //}
  
  
  //data generating process (likelihood)
	for (s in 1:NS) {
		for (t in 1:NT[s]) {
		  if (choice[s,t] > 0) {
		    if (t < 76) {
		    choice_two[s,t] ~ bernoulli_logit(b_mean[1] + b_mean[2]*(Q[s,t,stim[s,t],1]-Q[s,t,stim[s,t],2]));
		    }
		  }
		}
	}
}

generated quantities {
  //matrix[K,K] Omega;
  //matrix[K,K] Sigma;
  real log_lik[N-(25*NS)];
  real log_lik_new[25*NS];
  int n;
  int m;
  
  //array for simulated data
  //int<lower=0,upper=1> y_rep[N];
  //y_rep=rep_array(5,N);
  
  //array for log likelihood
  log_lik=rep_array(0,N-(25*NS));
  log_lik_new=rep_array(0,25*NS);

  //get correlation matrix from cholesky
  //Omega=multiply_lower_tri_self_transpose(Lcorr);
  
  //diag_matrix(b_sd)*Omega*diag_matrix(b_sd) to get covariance
  //Sigma=quad_form_diag(Omega,b_sd);
  
  n = 1;
  m = 1;
  for (s in 1:NS) {
    for (t in 1:NT[s]) {
        if (choice[s,t] > 0) {
          if (t <= 75) {
          log_lik[n]=bernoulli_logit_lpmf(choice_two[s,t] | 
          b_mean[1] + b_mean[2]*(Q[s,t,stim[s,t],1]-Q[s,t,stim[s,t],2]));
          //y_rep[n]=bernoulli_logit_rng(beta[s,1] + beta[s,2]*(Q[s,t,stim[s,t],1]-Q[s,t,stim[s,t],2]));
          n = n+1;
          }
          if (t > 75){
          log_lik_new[m]=bernoulli_logit_lpmf(choice_two[s,t] | 
          b_mean[1] + b_mean[2]*(Q[s,t,stim[s,t],1]-Q[s,t,stim[s,t],2]));
          //y_rep_new[m]=bernoulli_logit_rng(beta[s,1] + beta[s,2]*(Q[s,t,stim[s,t],1]-Q[s,t,stim[s,t],2]));
          m = m+1;  
        }
      }
    }
  }
}
