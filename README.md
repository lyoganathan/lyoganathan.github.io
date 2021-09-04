# lyoganathan.github.io

Made this using jekyll and the minimal mistakes theme: https://github.com/mmistakes/minimal-mistakes.

R blogs were done using blogdown: https://bookdown.org/yihui/blogdown/jekyll.html

blogdown doesn't really work well with Jekyll, so it takes some work to get it working. I use the blogdown::build_site() to convert from .Rmd to .md and although the command will through an error, it still generates a .md file that you can manually rename to follow Jekyll formating.


In order to get Latex working with the math, I had to follow this: https://www.janmeppe.com/blog/How-to-add-mathjax-to-minimal-mistakes/. You also have to use the $ notation instead of \\[\\].