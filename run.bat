stack exec -- t3gbLz -d "Teenage Mutant Ninja Turtles III - Radical Rescue (U).gb" 0x10070 0x40 "decompressedScoreGfx.gb"
stack exec -- t3gbLz -d "Teenage Mutant Ninja Turtles III - Radical Rescue (U).gb" 0xe737 0x1f4 "decompressedMapNt.bin"
stack exec -- t3gbLz -c "decompressedScoreGfxMaster.gb" "compressedScoreGfx.bin"
stack exec -- t3gbLz -c "decompressedMapNtMaster.bin" "compressedMapNt.bin"
stack exec -- t3gbLz -d "compressedMapNt.bin" 0x0 0x1F4 "reDecompressedMapNt.bin"
pause