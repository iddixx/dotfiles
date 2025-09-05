#!/bin/bash
a='scale=2;' 
a+=$(brightnessctl g)
a+=' / '
a+=$(brightnessctl m)
a+=' * 100'
calculations=$(bc -l <<< $a)
echo "($calculations+0.5)/1" | bc 


