#!/bin/bash
## created on 2023-02-21

#### enter description here

bwlim=500
rclone="$HOME/PROGRAMS/rclone"
config="$HOME/Documents/rclone.conf"
otheropt=" --checkers=20 --delete-before --stats=300s"
bwlimit="  --bwlimit=${bwlim}k"

# if [[ "$(hostname)" = "sagan" ]]; then 
#     echo "Upload all pdfs"
#     "${rclone}" ${otheropt} ${bwlimit} --config "$config" --include "*.pdf"  sync "$HOME/MANUSCRIPTS/2022_sdr_trends/" "lapauththanasis:/Trends"
# else
#     echo "This runs only on sagan"
# fi

echo "Upload all pdfs"
"${rclone}" ${otheropt} ${bwlimit} --config "$config" --max-depth 1 --include "*.{pdf,odt,Rmd}" copy "$HOME/MANUSCRIPTS/2022_sdr_trends/" "lapauththanasis:/Trends"

exit 0 
