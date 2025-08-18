#!/usr/bin/rdmd
import std.string;
import std.stdio;
import std.process;
import std.path;
import std.file;
import std.conv;
import std.traits;
import std.getopt;
import core.stdc.stdlib;
import std.algorithm.searching;
import core.sys.posix.sys.types;
import core.sys.posix.dirent;
import core.sys.posix.unistd;

struct link
{
    string from; // contains base name of file/directory/symlink
    string to;   // contains path to the link directory
};

struct conf_s
{
    string default_link_dir; /* directory to link files/directories/symlinks */
    string[] ignored_items;  /* files/directories/symlinks that won't link */
    link[] specific_links;   /* specific directories to link instead of 'default_link_dir' */

};


conf_s get_config()
{
    string home_dir = environment.get("HOME", "/home/domain");
    conf_s config = {
         default_link_dir: buildNormalizedPath(home_dir, ".config")
        ,ignored_items: ["run_dmenu.d", "bgs"]
        ,specific_links: [
            {
                 from: ".bashrc"
                ,to: buildNormalizedPath(home_dir)
            }
            ,{
                 from: ".inputrc"
                ,to: buildNormalizedPath(home_dir)
            }
            ,{
                 from: ".bash_aliases"
                ,to: buildNormalizedPath(home_dir)
            }
            ,{
                 from: ".stalonetrayrc"
                ,to: buildNormalizedPath(home_dir)
            }
            ,{
                 from: ".ratpoisonrc"
                ,to: buildNormalizedPath(home_dir)
            }
            ,{
                 from: ".xsessionrc"
                ,to: buildNormalizedPath(home_dir)
            }
            ,{
                 from: "ufetch"
                ,to: buildNormalizedPath(home_dir ~ "/.local/bin")
            }
            ,{
                 from: "system24.theme.css"
                ,to: buildNormalizedPath(home_dir ~ "/.config/vesktop/themes")
            }
        ]

    };
    return config;
}

link[] find_link_by_name(string base_entry_name);
string[] list_dir_base(string dir);
string[] list_dir(string dir, bool is_absolute_paths = true);
string[] list_dir(string dir, out string[] base_names, bool is_absolute_paths = true);

/* when remove_conflicting is true,
   it will remove all the entries with the same name,
   for example: if you link alacritty/ to the .config/,
   but it already has an alacritty directory,
   it will remove it before linking */
void link_by_config(conf_s config, string from = ".", bool remove_conflicting = true);

/* links flatpaks to the `link_to` directory.
   if strip_names is true, it links flatpak with stripped name,
   for example: com.discordorg.Discord, becomes Discord.
   remove_conflicting  works the same as in `link_by_config`*/
void link_flatpaks(string link_to, bool strip_names = true, bool remove_conflicting = true);

void main(string[] args)
{
    if(geteuid() == 0)
    {
        writeln("Don't run deploy.d as root!");
        exit(1);
    }
    string home_dir = environment.get("HOME", "/home/domain");

    bool f_link_flatpaks = false;
    bool f_skip_conflicting = false;
    bool f_only_link_flatpaks = false;

    auto flags = getopt(args,
            "link-flatpaks", &f_link_flatpaks
            ,"only-link-flatpaks", &f_only_link_flatpaks
            ,"skip-conflicting", &f_skip_conflicting
            );

    if(f_link_flatpaks || f_only_link_flatpaks)
    {
        string bin_dir = buildNormalizedPath(home_dir, ".local", "bin");
        link_flatpaks(bin_dir, ParameterDefaults!link_flatpaks[1], !f_skip_conflicting);
        if(f_only_link_flatpaks)
            return;
    }

    link_by_config(get_config(), ParameterDefaults!link_by_config[1], !f_skip_conflicting);
}

void link_by_config(conf_s config, string from = ".", bool remove_conflicting = true)
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
                    writeln("[INFO]: remove-conflicting is false, skipping " ~ entries[i]);
                    continue;
                }
            }
            try
            {
                symlink(current_entry, destination);
                writeln("Linked " ~ current_entry ~ " to " ~ destination);
            }
            catch(FileException error)
            {
                stderr.writeln(error.msg);
                exit(1);
            }
        }
    }

}

string flatpak_name_strip(string name)
{
    if(name.empty)
        return name;
    string[] result = name.split('.');
    return result[result.length-1];
}

void link_flatpaks(string link_to, bool strip_names = true, bool remove_conflicting = true)
{
    if(!exists(link_to) || !isDir(link_to))
        return;

    string home_dir = environment.get("HOME", "/home/domain");
    string[] flatpak_dirs = [
         buildNormalizedPath("/", "var", "lib", "flatpak", "exports", "bin")
        ,buildNormalizedPath(home_dir, ".local", "share", "flatpak", "exports", "bin")
    ];

    foreach(dir; flatpak_dirs)
    {
        if(!exists(dir))
            continue;

        string[] base_entries = list_dir_base(dir);
        foreach(entry; base_entries)
        {
            string absolute_entry = buildNormalizedPath(dir, entry);
            string name = entry;

            if(strip_names)
                name = flatpak_name_strip(name);

            string destination = buildNormalizedPath(link_to, name);

            if(remove_conflicting)
            {
                if(exists(destination))
                    executeShell("rm -rf " ~ destination);
            }
            else
            {
                writeln("[INFO]: remove-conflicting is false, skipping " ~ absolute_entry);
                continue;
            }

            try
            {
                symlink(absolute_entry, destination);
                writeln("Linked " ~ absolute_entry ~ " to " ~ destination);
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
        // TODO: make relative and absolute paths, work outside of dotfiles directory
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
        stderr.writeln("[ERROR]: cannot open " ~ dir_name);
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
        stderr.writeln("[ERROR]: cannot close " ~ dir_name);
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
