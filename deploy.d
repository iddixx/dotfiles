#!/usr/bin/rdmd
import std.string;
import std.stdio;
import std.process;
import std.path;
import std.file;
import std.conv;
import std.traits;
import core.stdc.stdlib;
import std.algorithm.searching;
import core.sys.posix.sys.types;
import core.sys.posix.dirent;

struct link
{
    string from; // contains base name of file/directory/symlink
    string to;   // contains path to the link direcotry
};

struct conf_s
{
    string default_link_dir; /* directory to link files/directories/symlinks */
    link[] specific_links;   /* specific directories to link instead of 'default_link_dir' */
    string[] ignored_items;  /* files/directories/symlinks that won't link */
};

/* you'll need only the 'link_all' function
   when remove_conflicting is true,
   it will remove all the entries with the same name,
   for example: if you link alacritty/ to the .config/,
   but it already has an alacritty directory,
   it will remove it before linking */
void link_all(conf_s config, string from = ".", bool remove_conflicting = true);

void main()
{
    string home_dir = environment.get("HOME", "/home/caralett");
    conf_s config = {
         default_link_dir: buildNormalizedPath(home_dir, ".config")
        ,specific_links: [
            {
                 from: ".bashrc"
                ,to: buildNormalizedPath(home_dir)
            }
            ,{
                 from: ".stalonetrayrc"
                ,to: buildNormalizedPath(home_dir)
            }
        ]
        ,ignored_items: []
    };

    link_all(config);
}


link[] find_link_by_name(string base_entry_name);
string[] list_dir_base(string dir);
string[] list_dir(string dir, bool is_absolute_paths = true);
string[] list_dir(string dir, out string[] base_names, bool is_absolute_paths = true);

void link_all(conf_s config, string from = ".", bool remove_conflicting = true)
{
    string[] base_entry_names;
    string[] entries = list_dir(from, base_entry_names);

    for(size_t i = 0; i < entries.length; ++i)
    {
        string current_entry = entries[i];
        if(canFind(config.ignored_items, base_entry_names[i]))
            continue;
        while(isSymlink(current_entry))
            current_entry = readLink(current_entry);

        auto specific_link = find_specific_link(config.specific_links, base_entry_names[i]);

        {
            string destination;
            if(!specific_link.empty)
                destination = buildNormalizedPath(specific_link[0].to, base_entry_names[i]);
            else
                destination = buildNormalizedPath(config.default_link_dir, base_entry_names[i]);

            if(exists(destination))
            {
                if(remove_conflicting)
                {
                    // hardcoded
                    executeShell("rm -rf " ~ destination);
                }
                else
                {
                    stderr.writeln(destination ~ " already exists");
                    exit(1);
                }
            }
            try
            {
                symlink(current_entry, destination);
            }
            catch(FileException error)
            {
                stderr.writeln(error.msg);
                exit(1);
            }
        }
    }

}

string[] list_dir(string dir_name, out string[] base_entries, bool is_absolute_paths = true)
{
    string[] base_names = list_dir_base(dir_name);
    base_entries = base_names;
    string[] result;
    foreach(name; base_names)
    {
        auto relative_entry_name = "." ~ dirSeparator ~ name;
        if(!is_absolute_paths)
            result[result.length++] = relative_entry_name; 
        else
            result[result.length++] = buildNormalizedPath(absolutePath(relative_entry_name), dir_name);
    }
    return result;
}

string[] list_dir(string dir, bool is_absolute_paths = true)
{
    string[] i_just_dont_fucking_care_about_this_parameter_lol;
    return list_dir(dir, i_just_dont_fucking_care_about_this_parameter_lol, is_absolute_paths);
}

string[] list_dir_base(string dir_name)
{
    static auto ignored_items = [
        "..", ".", ".git", ".gitignore", "README.md", baseName(__FILE__)
    ];

    DIR* dir = opendir(toStringz(dir_name));
    if(dir == null)
    {
        stderr.writeln(dir_name ~ " cannot open");
        exit(1);
    }

    string[] result;
    dirent* entry;
    while((entry = readdir(dir)) != null)
    {
        auto base_entry_name = to!string(entry.d_name).fromStringz();
        if(!canFind(ignored_items, base_entry_name))
        {
            result[result.length++] = base_entry_name; 
        }
    }
    
    if(closedir(dir) != 0)
    {
        stderr.writeln(dir_name ~ " cannot close");
        exit(1);
    }

    return result;
}

link[] find_specific_link(link[] links, string base_entry_name)
{
    foreach(link; links)
    {
        if(link.from == base_entry_name)
            return [link];
    }
    return [];
}

