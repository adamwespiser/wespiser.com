---
title: Year Of Arxiv Daily Digests
header: Year Of Arxiv Daily Digests, what I learned...
date: December 7, 2017
---

Everyday this year, between 1 and 4am an email from arxiv.org appeared in my inbox, with the title and abstract of every paper submitted the previous day in Artificial Intelligence, Computation and Language, Computers and Society, Human-Computer Interaction, Information Retrieval, Learning, Other Computer Science, Programming Languages, Software Engineering, Social and Information Networks.     


It's a lot. Today, 156 submissions. I don't read all the abstracts, just the title, then read into the abstract until I have a reason not to. Maybe every other day I finish a paper, but those are the topics needed to cover my interests in data science, software engineering, and start up technologies. For me, its a question of breadth and depth.    


#### Why I did It
Curiosity. But that's not a good enough answer! The first major reason is that I enjoy reading and scrutinizing papers, of all levels, and to be honest, Arxiv.org is a bit of a mixed bag. The next, is that I like searching for ideas that relate to what I'm doing at work, and inspire me to develop the skills I need to do a 'great' side project currently beyond my skills. There's been a lot of positive feedback while reading, and I find new information all the time that are tangentially related to work.

#### What I found
Over the course of the year I found a variety of interesting papers that have influenced my work, thinking, and that I otherwise just think are worth sharing. Here they are:    


#### January
[A Conceptual Introduction to Hamiltonian Monte Carlo](https://arxiv.org/pdf/1701.02434.pdf)
STAN, a probabilistic programming language used for bayesian statistics, uses Hamiltonian Monte Carlo, and this is the guide for understanding the algorithm with a differential geometry primer included. Betancourt provides a good overview, covering the concepts, important metrics used for debugging STAN, and even the mathematics behind Hamiltonian Systems and phase space. This is a fascinating paper from the perspective of applying physics to solve numerical problems alone, but what makes it great is the geometric intuition it provides when you need to get a STAN model to converge. In my experience, the intersection between statistical model, STAN implementation, and convergence provides the solution space for possible bayesian models in STAN, and this guide really helps understand the later two concepts.

#### April
[The Future of Ad Blocking: An Analytical Framework and New Techniques](http://randomwalker.info/publications/ad-blocking-framework-techniques.pdf) Ads are everywhere, and we are only becoming more conscience of the effect they have on our attention, web experience, and importantly, privacy. Blocking ads is popular, although existing solutions are technically rudimentary in their implementation. The authors discuss how the ad blocking may likely evolve, what technical game states will be encountered, and propose an interesting end game that consists of user software that can both actively block ads while obfuscating ad block detection. There is incredible demand for ad-blocking software, and this paper really spells out and interesting solution to a problem many of us face!


#### June
[Developing Bug-Free Machine Learning Systems With Formal Mathematics](https://arxiv.org/pdf/1706.08605.pdf)    
The authors here are trying to bridge the gap between building a machine learning system, and deploying that system in production. If you are unfamiliar with how difficult this is, its a problem of design opposites: in model develop you are looking for a solution and you aren't sure what exactly you'll end up with, and in production you need a fast, efficient, and ultimately reliable algorithm that will work safely every time. What's so fascinating, is that they built a programming language with theorem proving expressive enough to run a variety of models needed for exploring model space, which is inherently "safe" enough to run in production. This idea is certainly far from finished, but its an early example of how programming language theory can help solve some of the more difficult problems in industrial data science and machine learning. This idea is far from finished, and 2018 will hopefully see more work being done on this, with possible integrations with the major deep learning libraries.    

#### July
[Sparsity information and regularization in the horseshoe and other shrinkage priors](https://arxiv.org/pdf/1707.01694.pdf)
Regularization is important in machine learning, and lets us train models that efficiently use just the features needed for prediction. Known as sparsity for bayesian statistics, this problem is difficult in terms of both defining the proper theoretical distributions, and computationally estimating the distributions parameters with sampling techniques. This paper goes a long way by providing theoretically justified, and computationally converging priors that allow for sparsity constraints to easily be added to bayesian models in STAN. This is a huge breakthrough, and adds a significant new practical technique available for bayesian modeling in STAN. Where only strongly uninformative priors were available before, we now have the ability to select n out of k features to be used in the final bayesian model. This should make STAN a viable option for feature selection, potentially expanding its role in many data science projects.   

[Proxy Discrimination in Data-Driven Systems](https://arxiv.org/abs/1707.08120)
What is fairness? Is our process fair, are we? For regulated institutions and implementers with moral fiber, this is a vital questions. This paper defines the use of proxy variables in discriminatory machine learning systems using information theory, then develops pseudocode algorithms for testing the presence of proxy variable discrimination in automated decisions. I like this paper for two reasons: One, is that it provides a good, testable mathematical basis for a concept many of us building algorithms are familiar with, proxy discrimination, and two, reading this paper was my first exposure to the field of fairness, and all its corresponding and contradictory measures. Issues of fairness are only becoming more of a priority for stakeholders, and this paper gives you a jumping off point to determine the fairness of an existing process and its inputs.  

#### August
[Disintegration and Bayesian Inversion, Both Abstractly and Concretely](https://arxiv.org/pdf/1709.00322.pdf)    
A wonderful paper about manipulating probability distributions, including beautiful visualizations of probabilisitic manipulation. The paper solidifies the notion of a probability distribution in a formal language, the basis for [EfProb Library](https://efprob.cs.ru.nl/) in Python. Overall, this is a really interesting application of formal semantics, mathematics, and statistics that I found extremely educational and a joy to read! For the mathematics alone, this paper is definitely worth a browse, especially if you are building a a software system with probabilistic reasoning!   

#### October
[Stream Graphs and Link Streams for the Modeling of Interactions over Time](https://arxiv.org/abs/1710.04073)
This paper develops a formalism for dealing with graph interactions over time, which is both self-consistent, and compatible with graph theory. This provides a coherent framework, and subsequently develops a set of graph theory measures, for dealing with datasets in many operational domains. What caught my attention was how well the formalism describes the some of the operational and network data I see at work, and the elegance of the solution. It would be interesting to see some additional work done here with causality, as the formalism already describes temporal relationships so well.  


#### November
[A Tutorial on Canonical Correlation Methods](https://arxiv.org/pdf/1711.02391.pdf)
Canonical Correlation Analysis is a multivariate statistical technique to compare paired sets of variables, where each set can contain many measures. This is a very well written tutorial, from explaining the motivation and history of the technique, to formulating CCA and giving a proof of its solution via Lagrange Multipliers. For a reader familiar with Principal Components Analysis, or even just linear algebra, this is a surprisingly effective tutorial, and very much worth the time!

#### General Trends
The biggest trend I saw this year was "Deep Learning applied to problem X",([What really is Deep Learning?](https://arxiv.org/pdf/1711.03577.pdf)) there are numerous papers per day that just deal with neural networks, implemented in all of the major toolkits. There is a lot of noise here, but definitely some good work, and I'm especially looking forward to what comes out next year about the role of causality and information theory in neural network representations. [See top answer for list of Deep Learning pubs in 2017](https://www.quora.com/What-are-the-most-significant-machine-learning-advances-in-2017).      

The next big thing I saw was exploratory data analysis on Twitter: select some tweets, run a suite of NLP feature extraction tools, then perform a statistical analysis. There are a lot of similar papers trying to predict 'Fake News': using paired data sets, using crowdsourcing, a lot of approaches. Something I wasn't expecting to see, was a lot of survey paper asking software developers opinion and work related questions, like "What are your favorite tools?", "Why did you quit your last job?", etc. These are usually on the lower end of the rigor spectrum, and are often hit or miss in methodology. Nonetheless, there are often some interesting insights, however obvious the subject matter may be.   
Another interesting area was the application of formal methods to existing problems: two of these papers made the list, and I find myself constantly thinking about, developing, and refining notation while working on data science problem.


#### What I Learned
* What my areas of interest are, including programming languages, time series processing models, bayesian statistics, causality and information in deep learning, and formal methods applied to X.
* The most effective process for me is to read the daily digest of abstracts is: open the email, read the title and author, and continue through the abstract, onto the link. If interest is lost at any point, go to the next entry, else, bookmark the link. Next, I subsequently read all the bookmarks.
* Whatever your problem is, there is someone working on a similar problem, whose approach will benefit you. This held for the majority of the data science problems I encountered, even if all they had to offer was a perspective on how to optimize something I didn't have time to do!    
* Its important to quickly distill the essence of an idea, how the authors are approaching a problem, and what they hope to achieve. There are a ton of great ideas on Arvix.org, and you can quickly see a new perspective on something!
