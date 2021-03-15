import numpy as np
from scipy.optimize import fsolve
import matplotlib.pyplot as plt

def qualprior(cues,support,lb,ub):
    """This corresponds to f(S_i) in the text. lb and ub are the lower bound and upper bound probabilities onlookers ascribe to an individual being low quality in case of no support at all (lb) and high quality in case of unanimous support (ub)"""
    if cues == 'True':
        return lb + (ub-lb)*support
    else:
        return .5

def posteriorsuccess(cues,pH,pL,thetaH,thetaL,support,lb,ub):
    """This is the posterior probability that an individual is of type 1 given that they participated and were successful"""
    return pH*thetaH*qualprior(cues,support,lb,ub)/(pH*thetaH*qualprior(cues,support,lb,ub) + pL * thetaL*(1-qualprior(cues,support,lb,ub)))

def posteriorfailure(cues,pH,pL,thetaH,thetaL,support,lb,ub):
    """This is the posterior probability that an individual is of type 1 given that they participated and were unsuccessful"""
    return pH*(1-thetaH)*qualprior(cues,support,lb,ub)/(pH*(1-thetaH)*qualprior(cues,support,lb,ub) + pL * (1-thetaL)*(1-qualprior(cues,support,lb,ub)))

def posteriornosignal(pH,pL):
    """This is the posterior probability that an individual is of type 1 given that they did not participate"""
    return (1-pH)/((1-pH) + (1-pL))

def f(support):
    """governs how S_i affects payoffs if it does. Corresponds to g(S) in the text."""
    return support**2 + .1 
    #return 1

def payoff(t,signal,support,cost,feedback):
    """The payoff function"""
    if feedback=='True':
        return f(support)*t - cost*(signal==1)
    else:
        return t - cost*(signal==1)

def exppayoffsignal(q,pH,pL,thetaH,thetaL,support,lb,ub,cost,feedback,cues):
    """Expected payoff from signalling"""
    ps = thetaH*(q==1) + thetaL*(q==0) # prob of success
    tsuccess = posteriorsuccess(cues,pH,pL,thetaH,thetaL,support,lb,ub)
    tfailure = posteriorfailure(cues,pH,pL,thetaH,thetaL,support,lb,ub)
    return ps* payoff(tsuccess, 1, support, cost,feedback) + (1-ps)*payoff(tfailure,1,support,cost,feedback)

def exppayoffnosignal(pH,pL,support,cost,feedback):
    """Expected payoff from not signalling"""
    tnosignal = posteriornosignal(pH,pL)
    return payoff(tnosignal,0,support,cost,feedback) 


# equilibrium strategies

def indifferencepoint(pL,*args):
    """Function to solve the indifference between signalling and not signalling for semi-separating equilibria where type 0 is indifferent and therefore uses a mixed strategy"""
    thetaH, thetaL, support, lb, ub, costL, feedback, cues = args
    return exppayoffsignal(0, 1, pL, thetaH, thetaL, support, lb, ub, costL,feedback,cues) - exppayoffnosignal(1, pL, support,costL,feedback)

def findindifferencepoint(thetaH, thetaL, support, lb, ub, costL,feedback,cues):
    candidates = fsolve(indifferencepoint,0,args=(thetaH, thetaL, support, lb, ub, costL,feedback,cues))
    candidates = [c for c in candidates if c>=0 and c<=1]
    if len(candidates)>0:
        return candidates[0]
    else:
        return "no solution"

def decision(thetaH, thetaL, support, lb, ub, costH, costL,feedback,cues):
    # condition for all signaling
    if exppayoffsignal(0,1,1,thetaH,thetaL,support, lb, ub,costL,feedback,cues)>0:
        return (1,1)
    # condition for separating
    elif exppayoffsignal(1,1,0,thetaH,thetaL,support, lb, ub,costH,feedback,cues)>0 and exppayoffsignal(0,1,0,thetaH,thetaL,support, lb, ub,costL,feedback,cues)<=0:
        return (0,1)
    # condition for no one signaling -- sensitive to assumptions about out of equilibrium inferences!!
    elif findindifferencepoint(thetaH,thetaL,support, lb, ub,costL,feedback,cues)!='no solution':
        return (findindifferencepoint(thetaH,thetaL,support, lb, ub,costL,feedback,cues),1)
    elif payoff(1,1,support,costH,feedback) <0:
        return (0,0)
    else:
        'no resolution'


def plot_strategies(params):
    thetaH,thetaL,costH,costL,feedback,cues = params
    lb, ub = [.1,.9]
    strategies = [decision(thetaH, thetaL, i, lb, ub, costH, costL,feedback,cues) for i in np.arange(0,1,.05)]
    plt.plot(np.arange(0,1,.05),strategies)
    plt.legend(['low qual','high qual'])
    plt.show()
## 


def norm_dist(val,mean,noise):
    """Normal distribution pdf function"""
    return 1/(2*np.pi*noise**2)**(1/2)*(np.exp(-1/2*((val-mean)/noise)**2))

def draw_interaction_updates(j,n,mean_val,noise):
    """Function to provide an updated weight for j. j is the individual whom i is visiting and n is the number of visits from i to j"""
    weight_updates = 0
    for k in range(0,n):
        interaction_qual = np.random.normal(mean_val[j],noise) 
        weight_updates += norm_dist(interaction_qual,1,noise)/(norm_dist(interaction_qual,1,noise) + norm_dist(interaction_qual,0,noise)) ## posterior prob of being high qual
    return weight_updates

def draw_interaction_updates_twoways(i,j,n,mean_val,noise):
    """Function to provide an updated weight for both i and j when there is two-way learning. i is the visitor, j is the visited and n is the number of visits from i to j. In two ways, both visiter and visited learn about each other"""
    weight_updates_j = 0
    weight_updates_i = 0
    for k in range(0,n):
        interaction_qual_j = np.random.normal(mean_val[j],noise) 
        interaction_qual_i = np.random.normal(mean_val[i],noise)
        weight_updates_j += norm_dist(interaction_qual_j,1,noise)/(norm_dist(interaction_qual_j,1,noise) + norm_dist(interaction_qual_j,0,noise)) ## posterior prob of being high qual
        weight_updates_i += norm_dist(interaction_qual_i,1,noise)/(norm_dist(interaction_qual_i,1,noise) + norm_dist(interaction_qual_i,0,noise)) ## posterior prob of being high qual       
    return weight_updates_j,weight_updates_i

def bilateral_updates(N,weights,M, mean_val,noise, d):
    """ Function to draw pairwise interactions and the resulting update in weights when learning is only one-way.
    M is the number of interactions per person. d is the discount factor. if d=1, there is no discounting."""
    interaction_mat = np.zeros((N,N))
    for i in range(0,N):
        weight_vector = np.array(weights[i,:])
        denom = sum(weight_vector)
        probs = weight_vector/denom
        interactions = np.random.multinomial(M,probs) ## alternatively, could interact with top M weights.
        interaction_mat[i,:] = interactions
        weight_vector = np.array([d*weight_vector[i] + draw_interaction_updates(j,interactions[j],mean_val,noise) for j in range(0,N)])
        weight_vector[weight_vector<0]=0
        weights[i,:] = weight_vector
    return interaction_mat,weights

def bilateral_updates_twoways(N,weights,M, mean_val,noise, d):
    """ Function to draw pairwise interactions and the resulting update in weights when learning is two-way.
    M is the number of interactions per person. d is the discount factor. if d=1, there is no discounting."""
    interaction_mat = np.zeros((N,N))
    for i in range(0,N):
        weight_vector_visitor = np.array(weights[i,:]) # vector of weights from i to all j's
        weight_vector_visited = np.array(weights[:,i]) # vector of weights from all j's to i
        denom = sum(weight_vector_visitor)
        probs = weight_vector_visitor/denom
        interactions = np.random.multinomial(M,probs) ## alternatively, could interact with top M weights.
        interaction_mat[i,:] = interactions
        interaction_updates = [draw_interaction_updates_twoways(i,j,interactions[j],mean_val,noise) for j in range(0,N)]
        weight_vector_visitor = np.array([d*weight_vector_visitor[j] + interaction_updates[j][0] for j in range(0,N)])
        weight_vector_visited = np.array([d*weight_vector_visited[j] + interaction_updates[j][1] for j in range(0,N)])
        weight_vector_visitor[weight_vector_visitor<0]=0
        weight_vector_visited[weight_vector_visited<0]=0
        weights[i,:] = weight_vector_visitor
        weights[:,i] = weight_vector_visited
    return interaction_mat,weights


def S(N,weights):
    """ function determining S"""
    arr = np.max(weights,axis=1) ## what is the maximum weight that each j gives to other i's?
    probs = weights/arr[:,None] ## probability that individual j supports i is weight[j,i] over the maximum weight that j gives to any i
    num_support = np.zeros(N)
    for j in range(0,N):
        draws = np.random.random_sample(N)
        num_support[j] = sum(np.greater(probs[:,j],draws))
    maxsupport = max(num_support)
    minsupport = min(num_support)
    return (num_support-minsupport)/(maxsupport - minsupport)
