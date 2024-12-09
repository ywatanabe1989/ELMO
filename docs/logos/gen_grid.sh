#!/bin/bash
# Time-stamp: "2024-12-10 10:06:29 (ywatanabe)"
# File: ./ninja/docs/logos/gen_tile.sh

echo '<p align="center"><table>' > grid.html
for ((i=0; i<8; i++)); do
    echo "<tr>" >> grid.html
    for ((j=0; j<8; j++)); do
        num=$((i*8 + j + 1))
        printf "<td><img src=\"./docs/logos/logo_%02d.jpg\" width=\"100px\"></td>" $num >> grid.html
    done
    echo "</tr>" >> grid.html
done
echo '</table></p>' >> grid.html


# EOF
