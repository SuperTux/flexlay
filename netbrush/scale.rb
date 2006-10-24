#!/usr/bin/ruby -w

$scale = ARGV[0].to_f

$stdin.readlines.each{|i|
  lst = i.split[2..-1]
  if lst[0] == "dab" then
    print "#{lst[0]} #{lst[1]} #{lst[2].to_i * $scale} #{lst[3].to_i * $scale}"
  elsif lst[0] == "set_generic_brush" then
    print "#{lst[0]} #{lst[1]} #{lst[2].to_f * $scale} #{lst[3]} #{lst[4]} #{lst[5]} #{lst[6]} #{lst[7]}"
  else
    lst.each{|l| print "#{l} " }
  end
  puts
}

