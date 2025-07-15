#!/bin/bash
a='scale=2;' 
a+=$(brightnessctl g)
a+=' / 255 * 100'
calculations=$(bc -l <<< $a)
echo "($calculations+0.5)/1" | bc 


