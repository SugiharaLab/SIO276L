NUM_PLOTS=$1
list_of_plots=""
for i in `seq $NUM_PLOTS`
do
	plot="plots/${i}.png"
	list_of_plots="$list_of_plots $plot"
done
`convert $list_of_plots gifNetwork.gif`
