# Haskell Encoding Project

A Haskell implementation of Huffman encoding and Burrows-Wheeler transform for data compression.

## Building

To build the project, run:

```bash
cabal build
```

## Running

To compress a file:

```bash
cabal run haskell-encoding -- compress input.txt
```

To compress from stdin:

```bash
echo "Hello World" | cabal run haskell-encoding -- compress
```

To decompress a file:

```bash
cabal run haskell-encoding -- decompress compressed.txt
```

To decompress from stdin:

```bash
cat compressed.txt | cabal run haskell-encoding -- decompress
```

## Testing

To run the test suite:

```bash
cabal test
```

## Libraries Used

- `base`: Standard Haskell library
- `containers`: Provides Map and Set data structures
- `array`: Array operations (for future optimizations)
- `hspec`: Testing framework

## Project Structure

- `src/Huffman.hs`: Huffman encoding implementation
- `src/BurrowsWheeler.hs`: Burrows-Wheeler transform implementation  
- `src/Compression.hs`: Main compression interface
- `app/Main.hs`: Command-line interface
- `test/Spec.hs`: Test suite

## Current Status

This is a basic implementation. The compression format is simplified and doesn't yet store the Huffman tree needed for decompression. Future improvements would include:

1. Proper serialization of Huffman trees
2. More efficient BWT decoding
3. File I/O handling
4. Error handling