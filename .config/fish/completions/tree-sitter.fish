# Print an optspec for argparse to handle cmd's options that are independent of any subcommand.
function __fish_tree_sitter_global_optspecs
	string join \n h/help V/version
end

function __fish_tree_sitter_needs_command
	# Figure out if the current invocation already has a command.
	set -l cmd (commandline -opc)
	set -e cmd[1]
	argparse -s (__fish_tree_sitter_global_optspecs) -- $cmd 2>/dev/null
	or return
	if set -q argv[1]
		# Also print the command, so this can be used to figure out what it is.
		echo $argv[1]
		return 1
	end
	return 0
end

function __fish_tree_sitter_using_subcommand
	set -l cmd (__fish_tree_sitter_needs_command)
	test -z "$cmd"
	and return 1
	contains -- $cmd[1] $argv
end

complete -c tree-sitter -n "__fish_tree_sitter_needs_command" -s h -l help -d 'Print help'
complete -c tree-sitter -n "__fish_tree_sitter_needs_command" -s V -l version -d 'Print version'
complete -c tree-sitter -n "__fish_tree_sitter_needs_command" -f -a "init-config" -d 'Generate a default config file'
complete -c tree-sitter -n "__fish_tree_sitter_needs_command" -f -a "init" -d 'Initialize a grammar repository'
complete -c tree-sitter -n "__fish_tree_sitter_needs_command" -f -a "generate" -d 'Generate a parser'
complete -c tree-sitter -n "__fish_tree_sitter_needs_command" -f -a "build" -d 'Compile a parser'
complete -c tree-sitter -n "__fish_tree_sitter_needs_command" -f -a "parse" -d 'Parse files'
complete -c tree-sitter -n "__fish_tree_sitter_needs_command" -f -a "test" -d 'Run a parser\'s tests'
complete -c tree-sitter -n "__fish_tree_sitter_needs_command" -f -a "version" -d 'Increment the version of a grammar'
complete -c tree-sitter -n "__fish_tree_sitter_needs_command" -f -a "fuzz" -d 'Fuzz a parser'
complete -c tree-sitter -n "__fish_tree_sitter_needs_command" -f -a "query" -d 'Search files using a syntax tree query'
complete -c tree-sitter -n "__fish_tree_sitter_needs_command" -f -a "highlight" -d 'Highlight a file'
complete -c tree-sitter -n "__fish_tree_sitter_needs_command" -f -a "tags" -d 'Generate a list of tags'
complete -c tree-sitter -n "__fish_tree_sitter_needs_command" -f -a "playground" -d 'Start local playground for a parser in the browser'
complete -c tree-sitter -n "__fish_tree_sitter_needs_command" -f -a "dump-languages" -d 'Print info about all known language parsers'
complete -c tree-sitter -n "__fish_tree_sitter_needs_command" -f -a "complete" -d 'Generate shell completions'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand init-config" -s h -l help -d 'Print help'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand init" -s u -l update -d 'Update outdated files'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand init" -s h -l help -d 'Print help'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand generate" -l abi -d 'Select the language ABI version to generate (default 15). Use --abi=latest to generate the newest supported version (15).' -r
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand generate" -l libdir -d 'The path to the directory containing the parser library' -r
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand generate" -s o -l output -d 'The path to output the generated source files' -r
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand generate" -l report-states-for-rule -d 'Produce a report of the states for the given rule, use `-` to report every rule' -r
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand generate" -l js-runtime -d 'The name or path of the JavaScript runtime to use for generating parsers' -r
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand generate" -s l -l log -d 'Show debug log during generation'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand generate" -l no-bindings -d 'Deprecated (no-op)'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand generate" -s b -l build -d 'Compile all defined languages in the current dir'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand generate" -s 0 -l debug-build -d 'Compile a parser in debug mode'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand generate" -l json -d 'Report conflicts in a JSON format'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand generate" -s h -l help -d 'Print help'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand build" -s o -l output -d 'The path to output the compiled file' -r
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand build" -s w -l wasm -d 'Build a WASM module instead of a dynamic library'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand build" -s d -l docker -d 'Run emscripten via docker even if it is installed locally (only if building a WASM module with --wasm)'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand build" -l reuse-allocator -d 'Make the parser reuse the same allocator as the library'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand build" -s 0 -l debug -d 'Compile a parser in debug mode'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand build" -s h -l help -d 'Print help'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand parse" -l paths -d 'The path to a file with paths to source file(s)' -r -F
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand parse" -l scope -d 'Select a language by the scope instead of a file extension' -r
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand parse" -s d -l debug -d 'Show parsing debug log' -r -f -a "quiet\t''
normal\t''
pretty\t''"
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand parse" -l timeout -d 'Interrupt the parsing process by timeout (µs)' -r
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand parse" -l edits -d 'Apply edits in the format: \\"row, col delcount insert_text\\"' -r
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand parse" -l encoding -d 'The encoding of the input files' -r -f -a "utf8\t''
utf16-le\t''
utf16-be\t''"
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand parse" -l config-path -d 'The path to an alternative config.json file' -r -F
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand parse" -s n -l test-number -d 'Parse the contents of a specific test' -r
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand parse" -s 0 -l debug-build -d 'Compile a parser in debug mode'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand parse" -s D -l debug-graph -d 'Produce the log.html file with debug graphs'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand parse" -l wasm -d 'Compile parsers to wasm instead of native dynamic libraries'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand parse" -l dot -d 'Output the parse data with graphviz dot'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand parse" -s x -l xml -d 'Output the parse data in XML format'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand parse" -s c -l cst -d 'Output the parse data in a pretty-printed CST format'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand parse" -s s -l stat -d 'Show parsing statistic'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand parse" -s t -l time -d 'Measure execution time'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand parse" -s q -l quiet -d 'Suppress main output'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand parse" -l open-log -d 'Open `log.html` in the default browser, if `--debug-graph` is supplied'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand parse" -s j -l json -d 'Output parsing results in a JSON format'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand parse" -s r -l rebuild -d 'Force rebuild the parser'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand parse" -l no-ranges -d 'Omit ranges in the output'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand parse" -s h -l help -d 'Print help'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand test" -s i -l include -d 'Only run corpus test cases whose name matches the given regex' -r
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand test" -s e -l exclude -d 'Only run corpus test cases whose name does not match the given regex' -r
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand test" -l file-name -d 'Only run corpus test cases from from a given filename' -r
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand test" -l config-path -d 'The path to an alternative config.json file' -r -F
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand test" -l stat -d 'Show parsing statistics' -r -f -a "all\t''
outliers-and-total\t''
total-only\t''"
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand test" -s u -l update -d 'Update all syntax trees in corpus files with current parser output'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand test" -s d -l debug -d 'Show parsing debug log'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand test" -s 0 -l debug-build -d 'Compile a parser in debug mode'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand test" -s D -l debug-graph -d 'Produce the log.html file with debug graphs'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand test" -l wasm -d 'Compile parsers to wasm instead of native dynamic libraries'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand test" -l open-log -d 'Open `log.html` in the default browser, if `--debug-graph` is supplied'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand test" -l show-fields -d 'Force showing fields in test diffs'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand test" -s r -l rebuild -d 'Force rebuild the parser'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand test" -l overview-only -d 'Show only the pass-fail overview tree'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand test" -s h -l help -d 'Print help'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand version" -s h -l help -d 'Print help'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand fuzz" -s s -l skip -d 'List of test names to skip' -r
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand fuzz" -l subdir -d 'Subdirectory to the language' -r
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand fuzz" -l edits -d 'Maximum number of edits to perform per fuzz test' -r
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand fuzz" -l iterations -d 'Number of fuzzing iterations to run per test' -r
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand fuzz" -s i -l include -d 'Only fuzz corpus test cases whose name matches the given regex' -r
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand fuzz" -s e -l exclude -d 'Only fuzz corpus test cases whose name does not match the given regex' -r
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand fuzz" -l log-graphs -d 'Enable logging of graphs and input'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand fuzz" -s l -l log -d 'Enable parser logging'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand fuzz" -s r -l rebuild -d 'Force rebuild the parser'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand fuzz" -s h -l help -d 'Print help'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand query" -l paths -d 'The path to a file with paths to source file(s)' -r -F
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand query" -l byte-range -d 'The range of byte offsets in which the query will be executed' -r
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand query" -l row-range -d 'The range of rows in which the query will be executed' -r
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand query" -l scope -d 'Select a language by the scope instead of a file extension' -r
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand query" -l config-path -d 'The path to an alternative config.json file' -r -F
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand query" -s n -l test-number -d 'Query the contents of a specific test' -r
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand query" -s t -l time -d 'Measure execution time'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand query" -s q -l quiet -d 'Suppress main output'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand query" -s c -l captures -d 'Order by captures instead of matches'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand query" -l test -d 'Whether to run query tests or not'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand query" -s h -l help -d 'Print help'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand highlight" -l captures-path -d 'The path to a file with captures' -r -F
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand highlight" -l query-paths -d 'The paths to files with queries' -r -F
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand highlight" -l scope -d 'Select a language by the scope instead of a file extension' -r
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand highlight" -l paths -d 'The path to a file with paths to source file(s)' -r -F
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand highlight" -l config-path -d 'The path to an alternative config.json file' -r -F
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand highlight" -s n -l test-number -d 'Highlight the contents of a specific test' -r
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand highlight" -s H -l html -d 'Generate highlighting as an HTML document'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand highlight" -l css-classes -d 'When generating HTML, use css classes rather than inline styles'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand highlight" -l check -d 'Check that highlighting captures conform strictly to standards'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand highlight" -s t -l time -d 'Measure execution time'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand highlight" -s q -l quiet -d 'Suppress main output'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand highlight" -s h -l help -d 'Print help'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand tags" -l scope -d 'Select a language by the scope instead of a file extension' -r
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand tags" -l paths -d 'The path to a file with paths to source file(s)' -r -F
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand tags" -l config-path -d 'The path to an alternative config.json file' -r -F
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand tags" -s n -l test-number -d 'Generate tags from the contents of a specific test' -r
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand tags" -s t -l time -d 'Measure execution time'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand tags" -s q -l quiet -d 'Suppress main output'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand tags" -s h -l help -d 'Print help'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand playground" -l grammar-path -d 'Path to the directory containing the grammar and wasm files' -r
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand playground" -s q -l quiet -d 'Don\'t open in default browser'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand playground" -s h -l help -d 'Print help'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand dump-languages" -l config-path -d 'The path to an alternative config.json file' -r -F
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand dump-languages" -s h -l help -d 'Print help'
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand complete" -s s -l shell -d 'The shell to generate completions for' -r -f -a "bash\t''
elvish\t''
fish\t''
power-shell\t''
zsh\t''
nushell\t''"
complete -c tree-sitter -n "__fish_tree_sitter_using_subcommand complete" -s h -l help -d 'Print help'
