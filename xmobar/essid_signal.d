#!/bin/rdmd
import std.stdio;
import std.process;
import std.string;
import std.regex;
import std.conv;
import std.algorithm.searching;

void main()
{
    auto devices_info = executeShell("nmcli -t -f STATE,TYPE device status");
    auto devices_output = devices_info.output.splitLines();
    bool ethernet_connected = false;
    foreach(string line; devices_output)
    {
        if(line.canFind("ethernet") && line.canFind("connected"))
        {
            write("<fc=#00ff00>LAN connected</fc> <fc=#636363>|</fc> ");

            ethernet_connected = true;
        }
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
    if(!ethernet_connected)
    {
        write("<fc=#ff0000>No LAN</fc> <fc=#636363>|</fc> ");
    }
    if(!found_connected)
    {
        writeln("<fc=#ff0000>No Wi-Fi</fc>");
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
