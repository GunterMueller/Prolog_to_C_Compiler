# init-file for bones-generated programs

define dumpx
  x/16xg (BLOCK *)$arg0
end

define dumpxw
  x/16xw (BLOCK *)$arg0
end

define write
  call write_hook($arg0)
end

handle SIGPWR nostop pass
handle SIGXCPU nostop pass
