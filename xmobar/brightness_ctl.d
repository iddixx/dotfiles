#!/usr/bin/env rdmd
import std.algorithm;
import std.process;
import std.string;
import std.getopt;
import std.stdio;
import std.conv;

void main(string[] args)
{
    float current_brightness = to!float(filter!(a => a != '\n')(executeShell("brightnessctl g").output));
    float max_brightness = to!float(filter!(a => a != '\n')(executeShell("brightnessctl m").output));
    float percentage = (current_brightness / max_brightness * 100);

    writeln(to!int(percentage));
}

