Answers to questions from “Linux for Bioinformatics”
================
Ling Min Hao
17 September, 2021

## Orientation to Linux Files and Navigation

**Q1. What is your home directory?**

A: The home directory is /home/ubuntu.

**Q2. What is the output of this command?**

A: The output of this command is

``` r
# Output: 
# hello_world.txt
```

**Q3. What is the output of each `ls` command?**

A: There is no output for `my_folder`. The output of `ls` command for
`my_folder2` is

``` r
# Output: 
# hello_world.txt
```

**Q4. What is the output of each?**

A: There is no output for `my_folder` and `my_folder2`. The output of
`ls` command for `my_folder3` is

``` r
# Output: 
# hello_world.txt
```

## Scripting in the CLI

**Q5. What editor did you use and what was the command to save your file
changes**

A: I use `nano` editor. First, type CTRL + X. Then type Y. Finally press
Enter.

## Set up a protected sudoer account and connect with it

**Q6. What is the error?**

A: It gives an error message `Permission denied (public key)`. The main
problem is that there is no public key in the `authorized_keys` file
under `.ssh` directory in the instance.

**Q7. What was the solution?**

A: To solve this problem, we can set the appropriate ownership and file
permissions for the SSH directory and files contained in it, and append
the specified SSH public key to the `authorized_keys` file by following
the steps in this [link](https://www.youtube.com/watch?v=XfOsytNUq1w)
(Refer to second answer).

## Docker

**Q8. What does the `sudo docker run` part of the command do? and what
does the** **`salmon swim` part of the command do?**

A: The `sudo docker run` command first creates a writeable container
layer over the specified image, and then starts it using the specified
command. `salmon swim` perform super-secret operation.

## Set up a non-sudo user account

**Q9. What is the output of this command?**

A: The output of `sudo ls /root` is

``` r
# Output: 
# serveruser is not in the sudoers file.  This incident will be reported.
```

## Miniconda

**Q10. What is the output of `flask --version`?**

A: The output of `flask --version` is

``` r
# Output: 
# Python 3.9.5
# Flask 1.1.2 
# Werkzeug 1.0.1
```

**Q11. What is the output of `mamba -V`?**

A: The output of `mamba -V` is

``` r
# Output: 
# conda 4.10.3
```

**Q12. What is the output of `which python`?**

A: The output of `which python` is

``` r
# Output: 
# /home/serveruser/miniconda3/envs/py27/bin/python
```

**Q13. What is the output of `which python` now?**

A: The output of `which python` is

``` r
# Output: 
# /home/serveruser/miniconda3/bin/python
```

**Q14. What is the output of `salmon -h`?**

A: The output of `salmon -h` is

``` r
# Output: 
# salmon v1.4.0
# 
# Usage:  salmon -h|--help or 
#         salmon -v|--version or 
#         salmon -c|--cite or 
#         salmon [--no-version-check] <COMMAND> [-h | options]
# 
# Commands:
#      index      : create a salmon index
#      quant      : quantify a sample
#      alevin     : single cell analysis
#      swim       : perform super-secret operation
#      quantmerge : merge multiple quantifications into a single file
```

## Simple RNA-Seq analysis with `salmon`

### Part 1: Generating the transcriptome index

**Q15. What does the `-o athal.fa.gz` part of the command do?**

A: It saves the file downloaded from the url as `athal.fa.gz` into the
directory

**Q16. What is a `.gz` file?**

A: A GZ file is an archive file compressed by the standard GNU zip
(gzip) compression algorithm. It contains a compressed collection of one
or more files and is commonly used on Unix operating systems for file
compression.

### Part 2: Quantify RNA-Seq data

**Q17. What does the `zcat` command do?**

A: `zcat` is a command to view the contents of a compressed file without
literally uncompressing the file.

**Q18. What does the `head` command do?**

A: `head` command prints the top N number of data of the given input.

**Q19. What does the number `100` signify in the command?**

A: `-n` indicates line. `head -n N` means to print the first N lines
from the file. So, `head -n 100` means to print the first 100 lines from
the file.

**Q20. What is `|` doing? – Hint using `|` in Linux is called “piping”**

A: It first examines the `athanl.fa.gz` file using `zcat` and then print
the first 100 lines using `head -n 100`. A pipe is a form of redirection
that send the output of one command to another command for further
processing.

**Q21. What is a `.fa` file? What is this file format used for?**

A: The `.fa` file is a file formatted using the FASTA format that is
used to store nucleic acid sequence or protein sequence information.

**Q22. What format are the downloaded sequencing reads in?**

A: The folder named `SRR074122` is downloaded. Inside this folder, there
is `SRR074122.sra` file with `sra` extension known as Sequence Read
Archive, a bioinformatics database that provides public repository for
sequencing data. The filetype of `SRR074122.sra` is `data`.

**Q23. What is the total size of the disk?**

A: The total size of the disk is 7.7 G.

**Q24. How much space is remaining on the disk?**

A: The remaining space is 2.4 G.

**Q25. What went wrong?**

A: The storage is not enough to store the converted file.

**Q26. What was your solution? Hint: consider Q16**

A: Compress the output in `.gz` format.
