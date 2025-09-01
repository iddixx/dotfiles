#!/bin/rdmd
import std.stdio;
import std.process;
import std.string;
import std.regex;
import std.conv;
import std.file;

void main()
{
    string battery_charge = readText("/sys/class/power_supply/BAT0/capacity");
    string battery_status = readText("/sys/class/power_supply/BAT0/status");
    --battery_status.length;
    int battery_charge_int = parse!int(battery_charge);

    string pretty_battery_status;
    if((battery_status == "Charging") || (battery_status == "Full"))
    {
        pretty_battery_status = "(<fc=#00ff00>" ~ battery_status ~ "</fc>)";
    }
    else if(battery_status == "Not charging")
    {
        pretty_battery_status = "(<fc=#ffffff>" ~ battery_status ~ "</fc>)";
    }
    else if(battery_status == "Discharging")
    {
        pretty_battery_status = "(<fc=#ffff00>" ~ battery_status ~ "</fc>)";
    }

    if(battery_charge_int <= 30)
    {
        battery_charge = "<fc=#ff0000>" ~ to!string(battery_charge_int) ~ "%</fc>";
    }                                               
    else if(battery_charge_int < 70)              
    {                                               
        battery_charge = "<fc=#ffff00>" ~ to!string(battery_charge_int) ~ "%</fc>";
    }                                               
    else                                            
    {                                               
        battery_charge = "<fc=#00ff00>" ~ to!string(battery_charge_int) ~ "%</fc>";
    }


    write("BAT: " 
            ~ battery_charge
            ~ " "
            ~ pretty_battery_status);
}
