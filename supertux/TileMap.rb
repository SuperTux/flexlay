class TileMap
  attr_accessor :tilemaplayer

  def initialize()
  end

  def new_from_size(width, height)
    @width = width
    @height = height
    @tilemaplayer = TilemapLayer.new($tileset, @width, @height)
  end

  def parse(data)
    @width = get_value_from_tree(["width", "_"], data, 10)
    @height = get_value_from_tree(["height", "_"], data, 10)
    @layer = get_value_from_tree(["layer", "_"], data, "interactive")
    @solid = get_value_from_tree(["solid", "_"], data, true)
    @speed = get_value_from_tree(["speed", "_"], data, 1.0)
    @tilemaplayer = TilemapLayer.new($tileset, @width, @height)
    @tilemaplayer.set_data(get_value_from_tree(["tiles"], data, []))
  end

  def save(writer)
    writer.start_list("tilemap")
    writer.write_int("width", @width)
    writer.write_int("height", @height)
    writer.write_string("layer", @layer)
    writer.write_bool("solid", @solid)
    writer.write_float("speed", @speed)
    writer.write_int_vector("tiles", @tilemaplayer.get_data())
    writer.end_list("tilemap")
  end
end
