#!/bin/bash
# Time-stamp: "2024-12-10 10:10:07 (ywatanabe)"
# File: ./llemacs/docs/logos/gen_grid.sh

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


# <!-- <p align="center"><table>
#  !-- <tr>
#  !-- <td><img src="./docs/logos/logo_01.jpg" width="100px"></td><td><img src="./docs/logos/logo_02.jpg" width="100px"></td><td><img src="./docs/logos/logo_03.jpg" width="100px"></td><td><img src="./docs/logos/logo_04.jpg" width="100px"></td><td><img src="./docs/logos/logo_05.jpg" width="100px"></td><td><img src="./docs/logos/logo_06.jpg" width="100px"></td><td><img src="./docs/logos/logo_07.jpg" width="100px"></td><td><img src="./docs/logos/logo_08.jpg" width="100px"></td></tr>
#  !-- <tr>
#  !-- <td><img src="./docs/logos/logo_09.jpg" width="100px"></td><td><img src="./docs/logos/logo_10.jpg" width="100px"></td><td><img src="./docs/logos/logo_11.jpg" width="100px"></td><td><img src="./docs/logos/logo_12.jpg" width="100px"></td><td><img src="./docs/logos/logo_13.jpg" width="100px"></td><td><img src="./docs/logos/logo_14.jpg" width="100px"></td><td><img src="./docs/logos/logo_15.jpg" width="100px"></td><td><img src="./docs/logos/logo_16.jpg" width="100px"></td></tr>
#  !-- <tr>
#  !-- <td><img src="./docs/logos/logo_17.jpg" width="100px"></td><td><img src="./docs/logos/logo_18.jpg" width="100px"></td><td><img src="./docs/logos/logo_19.jpg" width="100px"></td><td><img src="./docs/logos/logo_20.jpg" width="100px"></td><td><img src="./docs/logos/logo_21.jpg" width="100px"></td><td><img src="./docs/logos/logo_22.jpg" width="100px"></td><td><img src="./docs/logos/logo_23.jpg" width="100px"></td><td><img src="./docs/logos/logo_24.jpg" width="100px"></td></tr>
#  !-- <tr>
#  !-- <td><img src="./docs/logos/logo_25.jpg" width="100px"></td><td><img src="./docs/logos/logo_26.jpg" width="100px"></td><td><img src="./docs/logos/logo_27.jpg" width="100px"></td><td><img src="./docs/logos/logo_28.jpg" width="100px"></td><td><img src="./docs/logos/logo_29.jpg" width="100px"></td><td><img src="./docs/logos/logo_30.jpg" width="100px"></td><td><img src="./docs/logos/logo_31.jpg" width="100px"></td><td><img src="./docs/logos/logo_32.jpg" width="100px"></td></tr>
#  !-- <tr>
#  !-- <td><img src="./docs/logos/logo_33.jpg" width="100px"></td><td><img src="./docs/logos/logo_34.jpg" width="100px"></td><td><img src="./docs/logos/logo_35.jpg" width="100px"></td><td><img src="./docs/logos/logo_36.jpg" width="100px"></td><td><img src="./docs/logos/logo_37.jpg" width="100px"></td><td><img src="./docs/logos/logo_38.jpg" width="100px"></td><td><img src="./docs/logos/logo_39.jpg" width="100px"></td><td><img src="./docs/logos/logo_40.jpg" width="100px"></td></tr>
#  !-- <tr>
#  !-- <td><img src="./docs/logos/logo_41.jpg" width="100px"></td><td><img src="./docs/logos/logo_42.jpg" width="100px"></td><td><img src="./docs/logos/logo_43.jpg" width="100px"></td><td><img src="./docs/logos/logo_44.jpg" width="100px"></td><td><img src="./docs/logos/logo_45.jpg" width="100px"></td><td><img src="./docs/logos/logo_46.jpg" width="100px"></td><td><img src="./docs/logos/logo_47.jpg" width="100px"></td><td><img src="./docs/logos/logo_48.jpg" width="100px"></td></tr>
#  !-- <tr>
#  !-- <td><img src="./docs/logos/logo_49.jpg" width="100px"></td><td><img src="./docs/logos/logo_50.jpg" width="100px"></td><td><img src="./docs/logos/logo_51.jpg" width="100px"></td><td><img src="./docs/logos/logo_52.jpg" width="100px"></td><td><img src="./docs/logos/logo_53.jpg" width="100px"></td><td><img src="./docs/logos/logo_54.jpg" width="100px"></td><td><img src="./docs/logos/logo_55.jpg" width="100px"></td><td><img src="./docs/logos/logo_56.jpg" width="100px"></td></tr>
#  !-- <tr>
#  !-- <td><img src="./docs/logos/logo_57.jpg" width="100px"></td><td><img src="./docs/logos/logo_58.jpg" width="100px"></td><td><img src="./docs/logos/logo_59.jpg" width="100px"></td><td><img src="./docs/logos/logo_60.jpg" width="100px"></td><td><img src="./docs/logos/logo_61.jpg" width="100px"></td><td><img src="./docs/logos/logo_62.jpg" width="100px"></td><td><img src="./docs/logos/logo_63.jpg" width="100px"></td><td><img src="./docs/logos/logo_64.jpg" width="100px"></td></tr>
#  !-- </table></p> -->

# EOF
