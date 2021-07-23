# Code to reproduce the ABM results of "When does reputation lie? Dynamic feedbacks between costly signals, social capital, and social prominence"

import numpy as np
import random
import scipy.stats
import pandas as pd 
from RepDynSignalingCues_Functions import *

## Initialization and parameters
np.random.seed(10)
N = 300
random_case = 'correlated' 
rep_diff = 1
weights = np.zeros((N,N))
if random_case == 'uncorrelated': ## uncorrelated removes the systematic initial bias giving some higher initial weights and social prominence
    weights = np.reshape(np.random.choice(range(10),N**2),(N,N)) ## rows are the agents sending a link/support and the columns are receiving it (visited/supported)
else:
    early_rep = np.random.choice(range(2),N)
    for i in range(N):
         weights[:,i] = np.random.choice(np.arange(0+rep_diff*early_rep[i],10-rep_diff*(1-early_rep[i])),N)

np.fill_diagonal(weights,0)
weights = weights.astype(float)
qual = np.random.choice([0,1],N) # the qualities of individuals 
mean_val = qual # the mean value of the interaction in pairwise interactions is equal to the qualities. To make pairwise interactions uninformative, set mean_val=[0,0] instead
noise = 1 # noise in the pairwise interaction
theta = [.8,.6] # probabilities of success
thetaH,thetaL = theta
costH, costL = [.2,.4] # costs of signalling
lb,ub = [.1,.9] # lower and upper bound of the priors on quality
M = 10 # number of people each individual chooses to interact with in the pairwise interactions
d = .98 # memory
Tmax = 300
history = np.zeros((Tmax,7,N)) ## record for each person their qual, number of interactions received, number of support, signaling probability, signaling decision, signaling outcome, reputation
# Turning mechanisms on and off to get different models. 
# cues = True and signalling = False and feedback = False gives the Cue only model
# cues = True and signalling = True and feedback = False gives signalling mechanism 1
# cues = False and signalling = True and feedback = True gives signalling mechanism 2
# cues = True and signalling = True and feedback = True gives signalling mechanisms 1 & 2
cues = 'True'
signaling = 'True'
feedback = 'True' # feedback means that S_i feeds back into the payoff function

## Simulation

for t in range(0,Tmax):
    history[t,0,:] = qual
    #pairwise interactions
    interactions,weights = bilateral_updates(N,weights,M, mean_val,noise, d) # use bilateral_updates_twoways to have two-way learning instead
    history[t,1,:] = np.sum(interactions,axis=0)
    # determination of social prominence revealed at public event
    S_vec = S(N,weights) 
    history[t,2,:] = S_vec
    if (cues == 'True') & (signaling =='False'):
        qhats = qualprior(cues,S_vec,lb,ub) # cues only: qhat = f(S_i)
        history[t,6,:] = qhats
        weights = d*weights + qhats[None,:]
    elif signaling =='True':
        qhats = np.zeros(N)
        for i in range(0,N):
            strategies = decision(thetaH, thetaL, S_vec[i], lb, ub, costH, costL,feedback,cues) ## determine the strategy profile for this value of S_i
            pL,pH = strategies 
            history[t,3,i] = strategies[qual[i]]
            if np.random.binomial(1,strategies[qual[i]]) == 1: # random draw if signalling is a mixed strategy
                history[t,4,i] = 1
                if np.random.binomial(1,theta[1-qual[i]]) == 1: # random draw for success of signal
                    history[t,5,i] = 1
                    qhat = posteriorsuccess(cues,pH,pL,thetaH,thetaL,S_vec[i],lb,ub)
                else:
                    history[t,5,i] = 0
                    qhat = posteriorfailure(cues,pH,pL,thetaH,thetaL,S_vec[i],lb,ub)
                history[t,6,i] = qhat
                qhats[i] = qhat
        weights = d*weights + qhats[None,:]


## Make dataframe to export to R
resultsdf = pd.DataFrame({'id':np.concatenate(tuple(([i]*Tmax for i in np.arange(N)))),'support':np.concatenate(tuple((history[:,2,i] for i in np.arange(N))),axis=None),'bilateral_interaction':np.concatenate(tuple((history[:,1,i] for i in np.arange(N))),axis=None),'time':[t for t in np.arange(Tmax)]*N,'qual':np.concatenate(tuple([qual[i]]*Tmax for i in np.arange(N))),'early_rep':np.concatenate(tuple([early_rep[i]]*Tmax for i in np.arange(N)))})
resultsdf.to_csv('resultsdf_cue{}_sig{}_fd{}.csv'.format(cues,signaling,feedback),index=False)

### Output to plot strategies
cues = 'True'
signaling = 'True'
feedback = 'False'
theta = [.8,.6]
thetaH,thetaL = theta
costH, costL = [.2,.4]
lb,ub = [.1,.9]
plot_strategies([thetaH,thetaL,costH,costL,feedback,cues])
support_grid = np.arange(0,1.05,0.05)
strategies = [decision(thetaH, thetaL, s, lb, ub, costH, costL,feedback,cues) for s in support_grid]
boostsuccess = [posteriorsuccess(cues,strategies[i][1],strategies[i][0],thetaH,thetaL,support_grid[i],lb,ub)-.5 if strategies[i][1] + strategies[i][0]>0 else 0 for i in range(len(support_grid)) ]
boostfailure = [posteriorfailure(cues,strategies[i][1],strategies[i][0],thetaH,thetaL,support_grid[i],lb,ub)-.5 if strategies[i][1] + strategies[i][0]>0 else 0 for i in range(len(support_grid))]
anal_results = pd.DataFrame({'s':support_grid,'pH':[strategies[i][1] for i in range(len(support_grid))],'pL':[strategies[i][0] for i in range(len(support_grid))],
    'rep_boost_success':boostsuccess,'rep_boost_failure':boostfailure})
anal_results.to_csv('analresults_cue{}_sig{}_fd{}_{}_{}_{}_{}.csv'.format(cues,signaling,feedback,thetaH,thetaL,costH,costL),index=False)


