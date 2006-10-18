#!/usr/bin/ruby -w

$scale = ARGV[0].to_i

$stdin.readlines.each{|i|
 lst = i.split[2..-1]
 if lst[0] == "dab" then
   print "#{lst[0]} #{lst[1]} #{lst[2].to_i * $scale} #{lst[3].to_i * $scale}"
   else
 lst.each{|l| print "#{l} " }
 end
 puts
}

