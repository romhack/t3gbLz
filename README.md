# t3gbLz.
LZSS compression tool for 'Teenage Mutant Ninja Turtles III - Radical Rescue (U)' game


Synopsis:
```
t3gbLz [-d | -c] inFileName outFileName
```

Description:
```

t3gbLz -d <inFile> <offset> <outSize> <outFile> - Decompress block from given ROM file.

t3gbLz -c <inFile> <outFile> - Compress given plain block.

-h - Display help

-v - Output version information
```

See additional files in [release](https://github.com/romhack/t3gbLz/releases/latest) archive.

Compression scheme is used at least for 2bpp tiles and nametables. Nothing special, just LZSS with some quirks in serialization scheme:
```
8 LZ entries are prefixed by flags byte.  
1 - literal copy;
0 - LZ pair: ZZZZZZZZ ?XXYYYYY where
XXZZZZZZZZ - lzDistance. 
	Unexplainable: buffer ring starts from 3DE and wraps to 0 at 400.
	In absolute address: D700-DAFF
YYYYY - lzLength -3
```
Build with [Haskell Stack](https://haskellstack.org) tool.
