haskell-algorithms-in-erlang
============================

Erlang versions of Haskell example code from Rabhi &amp; Lapalme's "Algorithms: A Functional Programming Approach" (Addison Wesley, 1999)

This is a simple-minded exercise I've been meaning to embark upon for, oh, twelve years or so. As will be evident to anyone who bothers to look, I'm a fairly naive Erlang coder and this is a self-guided tutorial for my edification. As a result this effort may provide some amusement to other, wiser, beings.  

My process is brutishly straightforward: rename the original Haskell source file, comment out the Haskell but leave it otherwise untouched, then attempt to emulate the algorithm and naming conventions as closely as possible, in an Erlang idiom (as I understand it!), on a per-function basis. Once it compiles I run perfunctory tests and move on. 

The subtle differentiation of the two language paradigms becomes more and more stark as I proceed into the depths of the book. The general skeletons of the algorithms are distinguishable, unsurprisingly, but the rich type system that Haskell offers underlies much of the book's depth, and - largely perhaps due to my own childlike approach to Erlang coding - that is literally lost in the translation! 

This may turn out to involve a grander process of iteration than I'd expected. Haskell is a fascinating language, and I'm delighted to be grappling with it even if in this somewhat oblique and sub-academic way.
 

===========================================================================
Here's the original note on the Haskell files by the authors, for reference:

The files in this directory are Haskell modules and examples of use
given in the book
            Algorithms: a functional programming approach
               Fethi A. Rabhi, University of Hull, UK
               Guy Lapalme, Université de Montréal, Canada

Each chapter has a subdirectory named after the number of the chapter:
Each file is a Haskell module of the same name as the file.

The convention for naming a file is as follows:
    SX_Y_Z.hs
   where X = chapter number
         Y = section number
         Z = subsection number (if needed)
...
The programs have been tested in October 1998 using Hugs 1.4
available at 
          http://haskell.org/hugs

