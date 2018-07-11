#!/bin/bash
# arg 1 : folder path 

in_fold=$1

#line numbers to modify 
usrFr_nums=( 7 20 33 46 59 72 85 98 111 124 137 150 163 176 189 202 215 228 241 254 267 280 293 306 319 332 345 358 )
TW_nums=( 11 24 37 50 63 76 89 102 115 128 141 154 167 180 193 206 219 232 245 258 271 284 297 310 323 336 349 362 )

echo "Reading usrFract file ..." 
IFS=$'\n' usrFract=($(cat $in_fold/usrFract.dat))
nlength=${#usrFract[@]}
ni=${!usrFract[@]}

echo "Reading TW values ...." 
IFS=$'\n' TW_vals=($(cat $in_fold/TW.dat))


echo "Inserting usrFrac values..." 
cp $in_fold/user_details.dat tmp
for i in {0..27}
do
awk -v line=${usrFr_nums[$i]} -v new_content="${usrFract[$i]}" '{
        if (NR == line) {
             printf new_content; printf "\n";  
                        } else {
print $0;
}
}' tmp > tmp2
cp tmp2 tmp
done


echo "Inserting TW values..." 
for i in {0..27}
do
awk -v line=${TW_nums[$i]} -v new_content="${TW_vals[$i]}" '{
        if (NR == line) {
             printf new_content; printf "\n";  
                        } else {
print $0;
}
}' tmp > tmp2
cp tmp2 tmp
done

#copy to output filename
cp tmp $in_fold/user_details.dat
rm tmp tmp2 
