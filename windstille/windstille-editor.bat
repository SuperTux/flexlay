if %already_run%.==yes. goto runit
set already_run=yes
set RUBYLIB=%RUBYLIB%;..\lib;..\ruby

:runit
REM edit this to point to wherever your ruby.exe is located (or delete the path altogether if ruby.exe is already in your PATH)
call c:\libraries\ruby\ruby.exe windstille.rb