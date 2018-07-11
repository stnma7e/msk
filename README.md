msk
===

basically implements the Shazam algorithm

reads in .WAV files, decomposes them into frequency domain, and then chooses a subset of frequencies to output as a "fingerprint"

change the file function in src/Main.rs to the filename of a .WAV file and the fingerprint of each chunk of the song will be output
- the default chunk size is 5000 per song
