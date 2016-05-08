# TreeGenerator

This TreeGenerator was built as an project along with Anusha Fatima () for our Functional Data Structures course in 4th Semester at Habib University.

## Introduction
Our goal was to basically develop a visualizer for data structures. Currently this supports Binary Trees, BSTs and RB Trees. 

"The user inputs the integers in a particular order and input which data strucuture they want and then our program will generate it's latex code for visualising it. When the code is complied, a visual description of the data is seen in the pdf document created for example an image of a binary trees with nodes , their values and their connections."


## Instructions on using the software

1. Load the Main.hs file in GHCI. Call the main function.
2. The user will then be prompted to chose the type of data structure they want. 1) Binary Tree, 2) BST and 3) Red Black Tree.
3. The user will then be prompted to enter a space seperated list of numbers. These numbers should be in a heap order (breadth-first order. Where the numbers are entered in the order root, left child, right child, left child of left, right child of left and so on...
4. The program will generate it's latex code in a file with the name <Tree Type>.tex where Tree Type is the type of data structure chosen by the user.
5. Convert this .tex file to .pdf using pdfLatex or any other software or online resource to visualize the data structure.


## Current Limitations.
- For the Binary Tree: the user can give at most 15 numbers, which means the tree can currently have at most 4 levels.

## Future Development Ideas
Here are a few list of possible improvements to this project, you're welcome to contribute!
- Allow for other data structures like 2-4 trees, hash tables etc.
- Remove the current restriction on Binary tree.
- Allow for performing of simple operations on a given tree such as rotation , insertion , removal etc.
- Give user the ability to construct incorrect BST and Red Black Trees.

## List of Contributors
Thank you everyone for all your contributions!
###Initial
1. **Anusha Fatima** (Creating of all the individual modules and relevant functions, developing code for user input, documentation and debugging)
2. **Syed Shariq Ali** (Created the module which outputs the actual latex code for each of the data structures, integrated all the modules together, documentation and debugging)
