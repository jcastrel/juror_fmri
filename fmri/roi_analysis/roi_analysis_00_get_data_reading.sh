#!/bin/sh
##ALREADY RUN##
#merge z-stat maps for each event (scenario,evidence) for each subject
#extract mean %signal change for each of the events (33 scenarios + 33 summed evidence) from significant clusters from univariate analysis
homedir=`pwd`
data_dir=${homedir}/data
mask_dir=${homedir}/mask


for i in {19933..20109}; do
	for ev in scenario evidence; do

		#first remove if already existing
		#rm ${data_dir}/${i}/${ev}/${i}_${ev}_list.txt
		#remove if already existing
		#rm ${data_dir}/${i}/${ev}/merged_${i}_${ev}.nii.gz

		#merge the list of files

		#echo "merging mean z-stat files for subject ${i} event ${ev}"
		#fslmerge -t ${data_dir}/${i}/${ev}/merged_${i}_${ev}.nii.gz `cat ${data_dir}/${i}/${ev}/${i}_${ev}_list.txt`

		#take the mean %signal change across trials (each timepoint is a a single scenario (or evidence), so 33 timepoints)
		for mask in avg_reading_evidence_mask; do
			echo "calculating mean z-stat for subject ${i} event ${ev} in the ${mask}"
			fslmeants -i ${data_dir}/${i}/${ev}/merged_${i}_${ev} -o ${data_dir}/${i}/${ev}/merged_${i}_${ev}_${mask}.txt -m ${mask_dir}/${mask};
		done;
	done
done
