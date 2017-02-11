---
title: "2.3 Lab: Introduciton to R"
output: html_notebook
---

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code. 
Introduction to functions. We'll create vectors x and y

```{r}
#### 2.3.1 Basic Commands ####
x=c(1,3,2,5)
x
x=c(1,6,2)
x
y=c(1,4,3)
y
```

What is the length of these vectors? The Length command gives us that information

```{r}
length(x)
length(y)
```

The ls() function gives us a list of all the objects we have saved so far.

```{r}
ls()
rm(x,y)
ls()
```

