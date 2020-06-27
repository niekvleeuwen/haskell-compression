# haskell-compression

In this repository, the compression algorithms run length and huffman are implemented in the Haskell programming language. Also IO is used to read and write to text files. 

## Run-length
Run-length encoding is the replacement of repeating patterns in data by the number of repeats plus what had to be repeated.

### Usage
Compress with run-length:
```
./rlcompress original.txt compressed.txt
```
Decompress with run-length:

```
./rldecompress compressed.txt original-restored.txt
```

## Huffman
Huffmancoding is a method to compress data consisting of a row of symbols optimally and losslessly. 

### Usage
Compress with huffman:

```
./huffmancompress original.txt compressed.txt tree.txt
```
Decompress with huffman:

```
./huffmandecompress compressed.txt original-restored.txt tree.txt
```