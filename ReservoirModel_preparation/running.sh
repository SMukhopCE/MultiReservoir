#!/bin/bash
# after running R file 'make_outputs.R' 

# change these only 
ntime=$1
foldpath=$2
monthstart=$3
monthend=$4

cd output_files

cp * $foldpath

cd ..

./TUNE.sh $foldpath

./TUNE_reser_details.sh $ntime $foldpath $monthstart $monthend

cd $foldpath

cp Release.dat decisionvar_details.dat

# change first entry of input.dat 
echo "Changing ntimesteps in input.dat ..." 

IFS=$'\n' input=($(cat input.dat))
echo "${input[@]}"

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





