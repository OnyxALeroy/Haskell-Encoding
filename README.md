# Haskell Encoding Project

A functional programming project implementing data compression algorithms in Haskell, including Huffman encoding, Burrows-Wheeler transform, and Run-Length encoding.

## Overview

This project implements a complete compression/decompression pipeline that combines multiple algorithms:
- **Huffman Encoding**: Lossless data compression based on frequency analysis
- **Burrows-Wheeler Transform**: Reversible transformation that groups similar characters
- **Run-Length Encoding**: Simple compression of consecutive repeated characters

## Features

- **Compression**: Apply BWT -> RLE -> Huffman encoding pipeline
- **Decompression**: Reverse the encoding process
- **File I/O**: Command-line interface for processing files
- **Custom Format**: Text-based storage format preserving all necessary metadata

## Installation

### Prerequisites

- GHC (Glasgow Haskell Compiler)
- Cabal (comes with GHC)
- Required libraries (automatically installed by Cabal):
  - `base`
  - `containers`
  - `array`
  - `hspec` (for testing)

### Building

```bash
cabal build
```

### Running Tests

```bash
cabal test
```

## Usage

### Command Line Interface

```bash
# Compress a file
cabal run haskell-encoding -- compress <input_file> <output_file>

# Decompress a file
cabal run haskell-encoding -- decompress <input_file> <output_file>
```

### Example

```bash
# Compress original.txt to encoded.txt
cabal run haskell-encoding -- compress original.txt encoded.txt

# Decompress encoded.txt back to decoded.txt
cabal run haskell-encoding -- decompress encoded.txt decoded.txt
```

## Architecture

### Modules

- **`Compression`**: Main compression/decompression pipeline
- **`Huffman`**: Huffman tree construction and encoding/decoding
- **`BurrowsWheeler`**: BWT transform and inverse transform
- **`RunLength`**: Run-length encoding and decoding
- **`Main`**: Command-line interface

### Compression Pipeline

1. **Burrows-Wheeler Transform**: Groups similar characters together
2. **Run-Length Encoding**: Compresses consecutive repetitions
3. **Huffman Encoding**: Final compression using frequency-based codes

### File Format

The compressed files use a custom text format:

```
FNS: BWT,RLE,HUFFMAN
SYMBOLS: A:0,B:100,C:1010,D:1011,E:11
POSITION: 3
DATA: 11101101110100000100100
```

- `FNS`: Functions applied during compression (in order)
- `SYMBOLS`: Huffman code table
- `POSITION`: Original text position for BWT inverse
- `DATA`: Compressed binary data as text

## Implementation Details

### Huffman Encoding

- Builds optimal prefix codes based on character frequencies
- Guarantees unique decodability through prefix property
- Tree construction using greedy algorithm

### Burrows-Wheeler Transform

- Generates all rotations of input string
- Sorts rotations lexicographically
- Outputs last column and original position
- Fully reversible through iterative reconstruction

### Run-Length Encoding

- Simple compression of consecutive identical characters
- Format: `count$character` (e.g., `5$a` for "aaaaa")
- Handles edge cases where no compression is beneficial

## Dependencies

- **base**: Core Haskell library
- **containers**: Data structures (Map, Set)
- **array**: Efficient array operations
- **hspec**: Testing framework
