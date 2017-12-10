This directory contains an example highlighting the difference between local thread memory and shared global memory.

Run this in an interactive R sessions with the required libraries.

Note that the difference between local and global memory access is an area of active hardware development.  On my relatively cheap laptop GPU, the difference in access speed is relatively large, ie 30x.  On a P100, it looks like it is in the 25% range.


