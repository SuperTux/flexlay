class LispWriter
  def initialize(file)
    @file = file
    @indent_depth = 0
  end

  def write_comment(comment)
    @file.write("; " + comment + "\n")
  end

  def start_list(listname)
    indent()
    @file.write("(" + listname + "\n")
    @indent_depth += 2
  end

  def end_list(listname)
    @indent_depth -= 2
    indent()
    @file.write(")\n")
  end

  def write_int(name, value)
    indent()
    @file.write("(%s %d)\n" % [name, value])
  end

  def write_float(name, value)
    indent()
    @file.write("(%s %f)\n" % [name, value])  
  end                                            

  def write_string(name, value, translatable = false)
    indent()
    @file.write("(" + name)
    if (translatable == true)
      @file.write(" (_ \"" + value + "\"))\n")
    else
      @file.write(" \"" + value + "\")\n")
    end
  end

  def write_bool(name, value)
    indent()
    @file.write("(" + name + " ")
    if(value == true)
      @file.write("#t")
    else
      @file.write("#f")
    end
    @file.write(")\n")
  end

  def write_int_vector(name, value)
    indent()
    @file.write("(" + name)
    for i in value
      @file.write(" %d" % [i])
    end
    @file.write(")\n")
  end

  def indent()
    @file.write(" " * @indent_depth)
  end
end
