##  $Id$
## 
##  Flexlay - A Generic 2D Game Editor
##  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
##
##  This program is free software; you can redistribute it and/or
##  modify it under the terms of the GNU General Public License
##  as published by the Free Software Foundation; either version 2
##  of the License, or (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
## 
##  You should have received a copy of the GNU General Public License
##  along with this program; if not, write to the Free Software
##  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

require "stringio"

def assoc_ref(lst, str)
  if lst == []
    return false
  elsif lst[0][0] == str
    return lst[0][1..-1]
  else
    return assoc_ref(lst[1..-1], str)
  end
end

def sexpr_filter(name, tree)
  ret = []
  for i in tree
    if i[0] == name
      ret.push(i[1..-1])
    end
  end
    
  return ret
end

def get_value_from_tree(spec, tree, default)
    if spec == []
      return tree
    elsif spec == ['_']
	  # is it a translatable string?
	  if(tree[0].instance_of?(Array) and tree[0][0] == "_")
		return tree[0][1]
	  else
        return tree[0]
	  end
    elsif tree == []
      return default
    else
      el = assoc_ref(tree, spec[0].to_sym)
      if el
        return get_value_from_tree(spec[1..-1], el, default)
      else
        return default
      end
    end
end

def write_sexpr(f, sexpr, indent = 0)
  if sexpr.is_a?(Array) then
    f.print "("
    sexpr.each_with_index{|e, i|
      write_sexpr(f, e)
      if i != sexpr.length() - 1 then
        f.print " "
      end
    }
    f.print ")"
  else
    if sexpr.is_a?(Symbol)
      f.print sexpr.to_s
    else
      f.print sexpr.inspect
    end
  end
end

class SExpression
  def initialize(sexpr = [])
    @sexpr = sexpr
  end

  def SExpression.new_from_file(filename)
    return SExpression.new(sexpr_read_from_file(filename))
  end

  def car()
    return SExpression.new(@sexpr[0])
  end

  def cdr()
    return SExpression.new((@sexpr[1..-1] or []))
  end

  def [](i)
    return SExpression.new(@sexpr[i])
  end

  # Interprets the SExpression as a AList in the form of ((name value) ...)
  def get(name, default)
    v = @sexpr.find() { |el| el[0] == name }
    if v then
      return SExpression.new(v[1..-1])
    else
      return SExpression.new(default)
    end
  end

  def value()
    return @sexpr
  end

  def get_value(spec, default = nil)
    return get_value_from_tree(spec, @sexpr, default)
  end

  def each_pair()
    @sexpr.each() { |el|
      yield(el[0], SExpression.new(el[1..-1]))
    }
  end

  def is_atom?()
    return @sexpr.is_a?(Array)
  end

  def is_nil?()
    return @sexpr == []
  end

  def to_s()
    str = StringIO.new()
    write(str)
    return str.string()
  end

  def to_a()
    return @sexpr
  end

  def write(f = $stdout, indent = 0)
    write_sexpr(f, @sexpr)
  end
end

# EOF #
