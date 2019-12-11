---
title: Deep Learning Koans
header: An Experiment In Educational Technology with Julia Programming Language and Flux.jl
date: December 11, 2019
---


#### Summary
- I built a library of of koans using the Julia programming language as part of a course project last semester! You can [Run them on Colab here](https://colab.research.google.com/github/adamwespiser/deep-learning-koans/blob/colab)
- The koans themselves are hosted on Jupyter Notebooks, and built from Julia source code with [Literate.jl](https://github.com/fredrikekre/Literate.jl)
- [Flux.jl](https://github.com/FluxML/Flux.jl) is deep learning library I used
- The koans are run locally, the [project Github repo is here](https://github.com/adamwespiser/deep-learning-koans)
- Although I've finished my first run of chapters, I'm still in the process of exploring these concepts, and would appreciate feedback!


#### Let's Begin with definitions!
First, a **koan** is a programming language problem with three aspects:
 - A text section that contains the introduction of a concept
 - A short snippet of no working code
 - A "test" or proof that will work once the understanding of the above concept has been used to fix the code

And **deep learning** is well, a buzzword, but I took it hear to mean a library, or API, that is capable of building modular neural networks using GPU acceleration. 

#### Literate.jl
Literate programming is the idea that your source code is both human readable, and machine runnable.
Literate.jl takes this up a step, and let's the user programatically build Jupyter notebooks from executable scripts. For instance, 

```
# # This would be a text
# right here is a continuation of that text

x_str = "now we are in a source code cell"
y = 1

# The first comment brings us back to text!
```
With that set up, building koans is pretty simple as we can alternate explanations with koans, and have the user interactively test and change their code.    

An example koan would be:
```
# # A Demo Koan
# array indexing in Julia is 1-based
xarray = ["a", "b", "c", "solution"]
ind = 0 # Fix me !
ind = 4 #src
@assert xarray[ind] == "solution"

```
And we can see a screen shot of the notebook.


<img src="/img/deep-learning-koans-demo-koan.png" style="height:auto; width:70%;">


Indeed, the `#src` tag will be filtered out by Literate.js when compiling the notebook, allowing the koan writer to test all the koans by sourcing the script, and the user to look up the solution in the source code if necessary. My script for generating the notebooks from Julia is [available here](https://github.com/adamwespiser/deep-learning-koans/blob/master/deps/build.jl), and you can find more information in [The Literate Julia Docs](https://fredrikekre.github.io/Literate.jl/latest/)


#### Flux.jl
Julia is a great programming language, and probably the best option for building neural networks for two reasons:
 - Julia runs fast, is gradually typed so you can write it fast, and compiles down to LLVM which means no calls to C/C++!
 - For the above reasons, a neural network can be programmed entirely with Julia, making differential programming much simpler, and building neural network easier!

Flux is still under active development, with notable improvements happening in the area of differential programming over the last year, and the next generation differentiation system, [Zygote](https://github.com/FluxML/Zygote.jl) is being integrated into Flux now!

So how do we teach Flux? My approach was to write 7 chapters, first covering Julia, then covering what I believed to be the the most important aspects of using a new DL library: working with data, building models, training models, using the GPU. 

My strategy was inspired by a project I did earlier this fall to [implement a variational auto-encoder in Flux](https://github.com/adamwespiser/variational-autoencoders), and I tried to create the document I would have wanted to read given my knowledge base (know math, ML, R/Python), if I were  to implement a similar project. If I get a chance, I'll talk about that project in another post!

Therefore, I set up 7 chapters, as collections of koans, in the following way:    
1. [Introduction to Julia](https://github.com/adamwespiser/deep-learning-koans/blob/master/notebooks/notebooks/t001_introduction.ipynb)    
2. [Working With Data In Julia](https://github.com/adamwespiser/deep-learning-koans/blob/master/notebooks/notebooks/t002_data_ingestion.ipynb)    
3. [Intro to Flux](https://github.com/adamwespiser/deep-learning-koans/blob/master/notebooks/notebooks/t003_flux_intro.ipynb)    
4. [Convolutional Neural Networks and Layers in Flux](https://github.com/adamwespiser/deep-learning-koans/blob/master/notebooks/notebooks/t004_conv_layers.ipynb)    
5. [Recurrent Neural Networks and Layers in Flux](https://github.com/adamwespiser/deep-learning-koans/blob/master/notebooks/notebooks/t005_recurrent_layers)    
6. [Flux optimization](https://github.com/adamwespiser/deep-learning-koans/blob/master/notebooks/notebooks/t006_optimization.ipynb)    
7. [Putting in all together, and more examples!](https://github.com/adamwespiser/deep-learning-koans/blob/master/notebooks/notebooks/t007_conclusion.ipynb)    


For the content of the koans, I wrote many of them myself, and was heavily inspired by the tutorial examples in the [Flux source code](https://github.com/FluxML/Flux.jl). 


#### Lessons Learned
Whether through the use of "koans", the chrome inspect tool, or the command line, if you are going to learn a new library, you need to play with it. Although I am not sure if koans via Jupyter notebook are here to stay, I think there is an acute need for easy ways to play around with code when you are trying to learn something new. Adding insight to this process should be the goal of any good koan writer!