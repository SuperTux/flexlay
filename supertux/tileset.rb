class TileGroup
  attr_accessor :name, :tiles
  
  def initialize(name, tiles)
    @name  = name
    @tiles = tiles
  end 
end

# Load game tiles from filename into tileset
class Tileset
  alias old_initialize initialize

  attr_accessor :tilegroups

  def initialize(*params)
    old_initialize(*params)

    @tilegroups = []
  end

  def load(filename)
    puts "Loading Tileset: #{filename}"
    tree = sexpr_read_from_file(filename)
    if tree == nil then
      puts "Error; Couldn't load: ", filename
      return 
    end

    tree = tree[1..-1]
    counter = 0
    
    tree.each do |i|
      case i[0]
      when "tile"
        data   = i[1..-1]
        id     = get_value_from_tree(['id', '_'], data, -1)
        image  = get_value_from_tree(['editor-images', '_'], data, false)
        hidden = get_value_from_tree(['hidden', '_'], data, false)

        # puts "Loading tile: #{id} => #{image}"

        if not(image)
          image = get_value_from_tree(['images', '_'], data, "notile.png")
        end
        
        if image.is_a?(String) then
          pixelbuffer = make_pixelbuffer($datadir + 'images/tilesets/' + image)
        elsif image.is_a?(Array) then
          if image[0] == "region" then
            pixelbuffer = make_region_pixelbuffer($datadir + 'images/tilesets/' + image[1],
                                                  image[2], image[3], image[4], image[5])
          end
        end
        
        if not hidden then
          if id == 0 then
            add_tile(id, nil)
          else
            add_tile(id, Tile.new(pixelbuffer))
          end
        end

      when "tilegroup"
        data  = i[1..-1]
        name  = get_value_from_tree(['name', '_'], data, "Unnamed")
        tiles = get_value_from_tree(['tiles'], data, [])
        
        if not @tilegroups then
          @tilegroups = []
        end
        @tilegroups.push(TileGroup.new(name, tiles))
      end

      counter += 1
      if counter % 20 == 0 then
        print "Loading tiles: %3.0f%%\r" % [counter.to_f/tree.length.to_f*100.0]
        $stdout.flush()
      end
    end
    puts ""
  end

  def create_ungrouped_tiles_group()
    @tilegroups.push(TileGroup.new("Ungrouped Tiles", get_ungrouped_tiles()))
  end

  def get_ungrouped_tiles()
    # Searches for tiles which are not yet grouped and creates a group
    # for them   
    # Potentially quite slow
    ungrouped_tiles = []
    get_tiles().each {|tile|
      catch :tile_is_grouped do
        tilegroups.each {|group|
          if group.tiles.index(tile) then
            throw :tile_is_grouped
          end
        }
        ungrouped_tiles.push(tile)
      end
    }
    return ungrouped_tiles
  end
end

# EOF #
