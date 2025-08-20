#!/bin/rdmd
import std.stdio;
import std.process;
import std.string;
import std.regex;
import std.conv;

void main()
{
    auto iwResult = executeShell("iwconfig wlan0 2>&1");
    if (iwResult.output.indexOf("no wireless extensions.") != -1)
    {
        writeln("wired");
        return;
    }

    auto nmResult = executeShell("nmcli dev wifi");
    auto lines = nmResult.output.splitLines();

    string essid = "";
    string signal = "";
    string rate = "";
    bool found_connected = false;
    foreach (line; lines) {
        if (line.startsWith("*"))
        {
            auto r = regex(` {2,}`);
            auto fields = line.split(r);
            if (fields.length >= 9)
            {
                essid = fields[2];
                signal = fields[6];
                rate =  fields[5];
            }
            found_connected = true;
            break;
        }
    }
    if(!found_connected)
    {
        writeln("<fc=#ff0000>No connection</fc>");
        return;
    }
    auto signal_int = parse!int(signal);
    string signal_string;
    if(signal_int <= 30)
    {
        signal_string = "<fc=#ff0000>Signal: " ~ to!string(signal_int) ~ "%</fc>";
    }                                               
    else if(signal_int < 70)              
    {                                               
        signal_string = "<fc=#ffff00>Signal: " ~ to!string(signal_int) ~ "%</fc>";
    }                                               
    else                                            
    {                                               
        signal_string = "<fc=#00ff00>Signal: " ~ to!string(signal_int) ~ "%</fc>";
    }
    writeln("<fc=#00ff00>ESSID: " 
            ~ essid
            ~ "</fc> <fc=#636363>|</fc> "
            ~ signal_string);
            // ~ " <fc=#636363>|</fc> "
            // ~ "Porn load speed: "
            // ~ rate);

}
