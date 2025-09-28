#!/usr/bin/env rdmd
import std.process;
import std.getopt;
import std.stdio;
import std.uni;

int main(string[] args)
{
    string cmd = "-fn 'Iosevka Fixed SS14 SemiBold:pixelsize=25:antialias=true' ";
    bool clipmenu = false;
    string theme = "";

    auto flags = getopt(args,
            "clipmenu", &clipmenu,
            "theme", &theme);

    if(toLower(theme) == "ram")
        cmd ~= "-nb '#61325C' -nf '#F7A0CB' -sb '#F9B7DD' -sf '#A853BB'";
    else if(toLower(theme) == "xmonad")
        cmd ~= "-nb '#000000' -nf '#FFFFFF' -sb '#6881b5' -sf '#000000'";
    if(clipmenu)
        return executeShell("clipmenu -b " ~ cmd).status;
    else
        return executeShell("export PATH=$HOME/.local/bin:$PATH ; dmenu_run " ~ cmd).status;
}
