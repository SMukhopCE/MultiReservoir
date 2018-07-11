#!/bin/bash
# arg 1 : ntime
# arg 2 : folder path/reservoir_details.dat 
# arg 3 : starting month number > range ( 1 .. 12 ) Jan  - Dec 
# arg 4 : end month number  > range ( 1 .. 12 ) Jan  - Dec

#line numbers to modify 
rc_nums=( 16 36 54 76 96 116 134 156 174 192 212 231 249 267 289 309 327 347 367 386 404 423 442 460 482 502 522 542 )
et_nums=( 17 37 55 77 97 117 135 157 175 193 213 232 250 268 290 310 328 348 368 387 405 424 443 461 483 503 523 543 )
sint_nums=( 5 23 43 61 83 103 123 141 163 181 199 219 238 256 274 296 316 334 354 374 393 411 430 449 467 489 509 529 )

# rule curve values
rc_val=( 1750 1633 1495 1280 1193 930 890 750 1350 858 744 660 1864 1672 1370 1190 1610 1395 750 657 1630 550 560 773 530 423 370 320 )
echo "Inserting rule curve values..." 
cp $2/reservoir_details.dat tmp
echo "Array size: ${#rc_nums[@]}"

for i in {0..27}
do
awk -v ntime=$1 -v line="${rc_nums[$i]}" -v new_content="${rc_val[$i]}" '{
        if (NR == line) {
             for(c=0;c<ntime;c++) printf("%.0d ", new_content); printf " \n";   
                        } else {
print $0;
}
}' tmp > tmp2
cp tmp2 tmp 
done 
rm tmp2 
echo "Reading ET_coeffs values..." 
et_coeff=( 0.00088 0.00126 0.00262 0.00442 0.00682 0.0078 0.00876 0.00752 0.00504 0.00308 0.00148 0.00088 )
nlength=${#et_coeff[@]}
echo "${et_coeff[@]}"
echo "ntime: $1"
echo "starting month: $3" 
echo "ending month: $4"
echo "re-arranging ET coefficients..." 
if [ $4 -gt $3 ]
 then 
 et_val=( "${et_coeff[@]:$(( $3-1 )):$(( $4-$3+1 ))}" )
 echo "${et_val[@]}"

 else 
 et_val=( "${et_coeff[@]:$(( $3-1 )):$(( $nlength-$3+1 ))}" "${et_coeff[@]:0:$(( $4 ))}" ) 
  echo "${et_val[@]}"
fi 
n=0;
nrep=$(( $1/12 ))
echo "$nrep"
if [[ $nrep -ne 0 ]]
then 
while [[ $n -lt $nrep ]]; 
do 
et_valX+=( "${et_val[@]}" );
n=$((n+1));
done 
echo "${et_valX[@]}"
else 
et_valX=${et_val[@]}
echo "${et_valX[@]}"
fi

for i in {0..27}
do
awk -v ntime=$1 -v line="${et_nums[$i]}" -v new_content="${et_valX[*]}" '{
        if (NR == line) {
             printf "%s\n",new_content;  
                        } else {
print $0;
}
}' tmp > tmp2
cp tmp2 tmp
done
rm tmp2

echo "Appending S_initial..." 

IFS=$'\n' s_init=($(cat $2/S_initial.dat))
echo "${s_init[@]}"

for i in {0..27}
do
awk -v line="${sint_nums[$i]}" -v new_content="${s_init[$i]}" -F " " '{
        if (NR == line) {
             sub($NF,new_content);print ; 
                        } else {
print $0;
}
}' tmp > tmp2
cp tmp2 tmp
done

##copy to output filename
cp tmp $2/reservoir_details.dat
rm tmp tmp2
#---------------------------------
#cp $outfile ./tmp.dat
# rule curve
#for i in 16 36
#do
#echo "$i"
#sed "16s/:.*/:$rep/" $outfile > test.dat
#done new_content; printf " \n" 
#i=0
##do
#nline=${rc_nums[$i]}
#rep=${rc_val[$i]}
#echo "$nline --- $rep"
#sed -i "${nline}s/.*/$rep/" tmp > tmp2
#cp tmp2 tmp
##done 
