def get_completions(text):
    import rlcompleter
    comp = rlcompleter.Completer()
    i = 0
    ret = []
    while True:
        line = comp.complete(text, i)
        if line == None:
            break;
        
        ret.append(line)           
        i += 1
    return ret

def run_python():
    repl = code.InteractiveConsole()
    repl.runsource("import readline")
    repl.runsource("import rlcompleter")
    repl.runsource("readline.parse_and_bind('tab: complete')")
    repl.interact("Use Ctrl-D to exit subshell")
    print "### Interactive Console finished"

# EOF #
