#!/bin/rdmd
import std.process;
import std.stdio;

// i wrote that script just because my laziness
// i'm lazy to change layout by hand, after launching dmenu
void main()
{
    /*executeShell("setxkbmap us -variant colemak_dh -option grp:shifts_toggle -option caps:capslock");*/
    executeShell("dmenu_run -fn 'Iosevka Fixed SS14 SemiBold:pixelsize=25:antialias=true' -nb '#61325C' -nf '#F7A0CB' -sb '#F9B7DD' -sf '#A853BB'");
    /*executeShell("setxkbmap us,ru,ua -variant colemak_dh,diktor,diktor -option grp:shifts_toggle -option caps:capslock");*/
    // didnt work ;(
}
