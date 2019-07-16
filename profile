#!/usr/bin/env tclsh
### compile + run program with profiling enabled-*- Tcl -*-

set opts "-DPROFILE"
set moreopts ""
set filename 0
set args ""
set i 0
set gprof 0

foreach x $argv {
    switch $x {
	-gprof {
	    set opts "-pg"
	    set gprof 1
	}
	-M {set opts "-DPROFILE_MEMORY"}
	default {
	    set filename $x
	    set args [lrange $argv [expr $i + 1] $argc] 
	    break
	}
    }
    set i [expr $i + 1]
}

if {$filename == 0} {
    puts "missing filename"
    exit 1
}

set bfile [file tail $filename]
set rfile [file rootname $bfile]
set xfile "tmp/${rfile}"
set cfile "${xfile}.c"

if {[file extension $filename] != ".c"} {
    exec ./pc -q -n $filename -o $cfile >@ stdout
}

eval exec gcc -std=gnu99 -Ilib -I. -g $opts $moreopts $cfile -o $xfile -lm -lrt >@ stdout
eval exec "./${xfile}" $args >@ stdout

if {$gprof} {
    exec gprof -b $xfile >@ stdout
} else {
    set pfile [lindex [glob PROFILE.*] 0]
    exec cat $pfile >@stdout
    file delete $pfile
}

