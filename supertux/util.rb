# Convenience function that loads a lisp file from disk and checks for a
# root symbol
def load_lisp(filename, root_symbol)
  tree = sexpr_read_from_file(filename)
  if tree == nil then
    raise "Error: Couldn't load '#{filename}'"
  end

  if tree[0] != root_symbol then
    raise "Error: '#{filename}' is not a '#{root_symbol}' file"
  end

  return tree
end

def load_cl_sprite(filename)
  if filename[-4..-1] == ".png"
    sprite = make_sprite(filename)
  elsif filename[-7..-1] == ".sprite"
    supertux_sprite = Sprite.new(filename)
    sprite = supertux_sprite.get_cl_sprite()
  else
    raise "Unsupported sprite format '#{spritefile}'"
  end
  
  return sprite
end
