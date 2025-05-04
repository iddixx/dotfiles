import os
import sys

from barpyrus import hlwm
from barpyrus import widgets as W
from barpyrus.core import Theme
from barpyrus import lemonbar
from barpyrus import conky
from barpyrus import trayer
from barpyrus.conky import col_fmt
from barpyrus.colors import (
    MY_BG        ,
    MY_BG_BRIGHT ,
    MY_FG        ,
    OTHER_FG     ,
    MY_ACTIVE_FG ,
    MY_PINK     ,
    MY_WHITE,
    FULL_WHITE,
    GREEN_FG   ,
    DARKER_FG    ,
    MY_ORANGE    ,
    AQUA_LIGHT,
    GREEN_LIGHT,
    YELLOW_LIGHT,
    PURPLE_LIGHT,
    BLUE_LIGHT,
    ORANGE_LIGHT,
    RED_LIGHT,
    FG,
    BG,
)

# Copy this config to ~/.config/barpyrus/config.py

# set up a connection to herbstluftwm in order to get events
# and in order to call herbstclient commands
hc = hlwm.connect()

# get the geometry of the monitor
monitor = sys.argv[1] if len(sys.argv) >= 2 else 0
(x, y, monitor_w, monitor_h) = hc.monitor_rect(monitor)
height = 32 # height of the panel
width = monitor_w  # width of the panel
hc(['pad', str(monitor), str(height)])  # get space for the panel

# Conky setup
custom = ''
separator = col_fmt(OTHER_FG) + '|';

##########
# volume #
##########

volume = col_fmt(MY_FG) + 'Volume: ${pa_sink_volume}%'

###########
# battery #
###########
battery = col_fmt(MY_FG) + "${if_existing /sys/class/power_supply/BAT0}"
battery += "%{T2}"
battery += "%{T-}Battery ";
battery += "${if_match \"$battery\" == \"discharging $battery_percent%\"}"
battery += col_fmt(MY_ORANGE) + "(No power supply)" 
battery += "$else"
battery += col_fmt(GREEN_FG) + "(Charging)" 
battery += "$endif"

battery += col_fmt(MY_FG) + ": $battery_percent%"
battery += "${endif}"

conky_elements = [
    custom,
    volume,
    separator,
    battery,
    separator
]

conky_text = ' '.join(conky_elements) + ' '

# you can define custom themes
grey_frame = Theme(bg=BG, fg=MY_FG, padding=(3, 3))
pink_frame = Theme(bg=FULL_WHITE, fg=MY_PINK, padding=(3, 3))

# Widget configuration:
font = 'Iosevka Fixed SS14 SemiBold:size=16'
symbol_font = '-wuncon-siji-medium-r-normal--10-100-75-75-c-80-iso10646-1'
bar = lemonbar.Lemonbar(
    geometry=(x, y, width, height),
    font=font,
    symbol_font=symbol_font,
    background = MY_BG,
    foreground = MY_FG
)
update_interval = '0.5'

bar.widget = W.ListLayout([
    W.RawLabel('%{l}'),
    hlwm.HLWMTags(hc, monitor, tag_renderer=hlwm.underlined_tags),

    # hlwm.HLWMTags(hc, monitor),
    W.RawLabel('%{c}'),

    pink_frame(W.DateTime(' %d %B, %H:%M ')),

    # hlwm.HLWMMonitorFocusLayout(hc, monitor,
    #        # this widget is shown on the focused monitor:
    #        grey_frame(hlwm.HLWMWindowTitle(hc)),
    #        # this widget is shown on all unfocused monitors:
    #        conky.ConkyWidget('df /: ${fs_used_perc /}%'),
    #        # conky.ConkyWidget(' '),
    # ),
    W.RawLabel('%{r}'),

    conky.ConkyWidget(
        text=conky_text,
        config={'update_interval': update_interval},
    ),

    trayer.StalonetrayWidget([x, y, width, height]),
    # something like a tabbed widget with the tab labels '>' and '<'

])
