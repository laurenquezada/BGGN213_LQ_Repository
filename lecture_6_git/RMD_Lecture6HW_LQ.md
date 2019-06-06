RMD\_Lecture6HW\_LQ
================
LQ
5/1/2019

### Can you improve this analysis code?

#### Original copy/paste to test functionality

``` r
library(bio3d)
s1 <- read.pdb("4AKE")  # kinase with drug
```

    ##   Note: Accessing on-line PDB file

``` r
s2 <- read.pdb("1AKE")  # kinase no drug
```

    ##   Note: Accessing on-line PDB file
    ##    PDB has ALT records, taking A only, rm.alt=TRUE

``` r
s3 <- read.pdb("1E4Y")  # kinase with drug
```

    ##   Note: Accessing on-line PDB file

``` r
s1.chainA <- trim.pdb(s1, chain="A", elety="CA")
s2.chainA <- trim.pdb(s2, chain="A", elety="CA")
s3.chainA <- trim.pdb(s1, chain="A", elety="CA")
s1.b <- s1.chainA$atom$b
s2.b <- s2.chainA$atom$b
s3.b <- s3.chainA$atom$b
plotb3(s1.b, sse=s1.chainA, typ="l", ylab="Bfactor")
```

![](RMD_Lecture6HW_LQ_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
plotb3(s2.b, sse=s2.chainA, typ="l", ylab="Bfactor")
```

![](RMD_Lecture6HW_LQ_files/figure-markdown_github/unnamed-chunk-1-2.png)

``` r
plotb3(s3.b, sse=s3.chainA, typ="l", ylab="Bfactor")
```

![](RMD_Lecture6HW_LQ_files/figure-markdown_github/unnamed-chunk-1-3.png)

#### Fixing copy/paste errors

``` r
s1 <- read.pdb("4AKE")  # kinase with drug
```

    ##   Note: Accessing on-line PDB file

    ## Warning in get.pdb(file, path = tempdir(), verbose = FALSE): /var/folders/
    ## 1_/_n1fc0xn4wb95r42s_j98wmh0000gp/T//RtmpsrnIki/4AKE.pdb exists. Skipping
    ## download

``` r
s2 <- read.pdb("1AKE")  # kinase no drug
```

    ##   Note: Accessing on-line PDB file

    ## Warning in get.pdb(file, path = tempdir(), verbose = FALSE): /var/folders/
    ## 1_/_n1fc0xn4wb95r42s_j98wmh0000gp/T//RtmpsrnIki/1AKE.pdb exists. Skipping
    ## download

    ##    PDB has ALT records, taking A only, rm.alt=TRUE

``` r
s3 <- read.pdb("1E4Y")  # kinase with drug
```

    ##   Note: Accessing on-line PDB file

    ## Warning in get.pdb(file, path = tempdir(), verbose = FALSE): /var/folders/
    ## 1_/_n1fc0xn4wb95r42s_j98wmh0000gp/T//RtmpsrnIki/1E4Y.pdb exists. Skipping
    ## download

``` r
s1.chainA <- trim.pdb(s1, chain="A", elety="CA")
s2.chainA <- trim.pdb(s2, chain="A", elety="CA")
s3.chainA <- trim.pdb(s3, chain="A", elety="CA")
s1.b <- s1.chainA$atom$b
s2.b <- s2.chainA$atom$b
s3.b <- s3.chainA$atom$b
plotb3(s1.b, sse=s1.chainA, typ="l", ylab="Bfactor")
```

![](RMD_Lecture6HW_LQ_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
plotb3(s2.b, sse=s2.chainA, typ="l", ylab="Bfactor")
```

![](RMD_Lecture6HW_LQ_files/figure-markdown_github/unnamed-chunk-2-2.png)

``` r
plotb3(s3.b, sse=s3.chainA, typ="l", ylab="Bfactor")
```

![](RMD_Lecture6HW_LQ_files/figure-markdown_github/unnamed-chunk-2-3.png)

#### **Q6. How would you generalize the original code above to work with any set of input protein structures?**

``` r
x <- "4AKE"
lq_protein <- function(x) {
  #Input: "x", the desired protein; function: calls in the pdb file; output: file of class pdb
  read_x <- read.pdb(x)
  #Input: "read_x", the named output of the read function; function: trims the original pdb file to only read (and provide a name vector) of the alpha carbons on the alpha chain; output: trimmed pdb file
  trim_x <- trim.pdb(read_x, chain="A", elety="CA")
  #Input: "trim_x", the trimmed output of the original pdb file; function: selects the atoms and b factors from the trimmed file; output: 
  chainatom_x <- trim_x$atom$b
  #Input: "chainatom_x", the final selection of the original pdb file; function: uses the bio3d package to plot our trimmed and selected data of the desired protein; output: specified plot
  plotb3(chainatom_x, sse = trim_x, typ="l", ylab="Bfactor")
}

lq_protein(x)
```

    ##   Note: Accessing on-line PDB file

    ## Warning in get.pdb(file, path = tempdir(), verbose = FALSE): /var/folders/
    ## 1_/_n1fc0xn4wb95r42s_j98wmh0000gp/T//RtmpsrnIki/4AKE.pdb exists. Skipping
    ## download

![](RMD_Lecture6HW_LQ_files/figure-markdown_github/unnamed-chunk-3-1.png)

### *Questions*

**Q1. What type of object is returned from the read.pdb() function?** *The function returns a list of properties of a given protein such as: a data.frame of atomic information, the protein sequence and secondary structure information.*

**Q2. What does the trim.pdb() function do?** *trim.pdb() literally trims the data return and creates a new PDB object of the protein that you are calling to a subset of atoms that you specify in the function.*

**Q3. What input parameter would turn off the marginal black and grey rectangles in the plots and what do they represent in this case?** *The black and grey rectangles represent secondary structures present throughout the selected residues. In the output plot, gray represents the beta sheets while black represents the alpha helices. To turn off the display of the rectangles, you would change the "top" and "bot" parameters to FALSE. You could also simply remove the sse parameter from the plotb3 function.*

**Q4. What would be a better plot to compare across the different proteins?** *A clustering plot (cluster dendrogram) resulting from hierarchical clustering of the data.*

**Q5. Which proteins are more similar to each other in their B-factor trends. How could you quantify this?** *The s2 and s3 functions are the most similar. The height of the connection of each of the branches represents how similar the proteins are in terms of their B-factor trends.*

``` r
hc <- hclust( dist( rbind(s1.b, s2.b, s3.b) ) )
plot(hc)
```

![](RMD_Lecture6HW_LQ_files/figure-markdown_github/unnamed-chunk-4-1.png)
