class TileGroup
  attr_accessor :name, :tiles
  
  def initialize(name, tiles)
    @name  = name
    @tiles = tiles
  end 
end

# Resolve a tiles.strf image / region spec to a 32x32 pixel buffer.
# +image+ may be a String path or a (:region file x y w h) list.
# +col+/+row+ index into a multi-tile sheet (used by the :tiles form).
def tile_image_region(image, col = 0, row = 0)
  base = ($datadir || "") + "images/"
  if image.is_a?(String)
    make_region_pixelbuffer_from_resource(base + image, col * 32, row * 32, 32, 32)
  elsif image.is_a?(Array) && image[0] == :region
    file = image[1]
    rx, ry = image[2].to_i, image[3].to_i
    # Sheet cells are still 32x32 relative to the region origin.
    make_region_pixelbuffer_from_resource(base + file, rx + col * 32, ry + row * 32, 32, 32)
  elsif image.is_a?(Array) && image[0].is_a?(String)
    # (images "a.png" "b.png" ...) — use first path as sheet
    make_region_pixelbuffer_from_resource(base + image[0], col * 32, row * 32, 32, 32)
  else
    warn "tileset: unsupported image form: #{image.inspect}"
    nil
  end
end

# Load game tiles from filename into tileset.
# Open Flexlay_wrap::Tileset explicitly so methods apply to factory-
# created instances (Tileset_new), not a parallel constant.
module TilesetInit
  def initialize(*params)
    super(*params)
    @tilegroups = []
  end
end
Flexlay_wrap::Tileset.prepend(TilesetInit) if defined?(Flexlay_wrap::Tileset)

class Flexlay_wrap::Tileset
  attr_accessor :tilegroups

  def load(filename)
    puts "Loading Tileset: #{filename}"
    tree = load_lisp(filename, :"supertux-tiles")

    tree = tree[1..-1]
    counter = 0
    
    tree.each do |i|
      case i[0]
      when :tiles
        data   = i[1..-1]
        width  = get_value_from_tree(['width', '_'], data, 1)
        _height = get_value_from_tree(['height', '_'], data, 1)
        ids    = get_value_from_tree(['ids'], data, [])
        # attributes = get_value_from_tree(['attributes'], data, [])
        image  = get_value_from_tree(['image', '_'], data, nil)

        if not image then
          image  = get_value_from_tree(['images', '_'], data, nil)
        end

        if not image then
          image  = get_value_from_tree(['editor-images', '_'], data, "tiles/auxiliary/notile.png")
        end

        # Prefer editor-images when present (may be region form).
        editor_image = get_value_from_tree(['editor-images', '_'], data, nil)
        image = editor_image if editor_image

        x = 0
        y = 0
        ids.each{|id|
          pixelbuffer = tile_image_region(image, x, y)
          add_tile(id, pixelbuffer ? Tile.new(pixelbuffer) : nil)
          x += 1
          if (x == width) then
            x = 0
            y += 1
          end
        }
        
      when :tile
        data   = i[1..-1]
        id     = get_value_from_tree(['id', '_'], data, -1)
        image  = get_value_from_tree(['editor-images', '_'], data, false)
        hidden = get_value_from_tree(['hidden', '_'], data, false)

        if not(image) then
          image = get_value_from_tree(['images', '_'], data, "tiles/auxiliary/notile.png")
        end

        pixelbuffer = nil
        if image.is_a?(String) then
          pixelbuffer = make_pixelbuffer(($datadir || "") + 'images/' + image)
        elsif image.is_a?(Array) && image[0] == :region then
          pixelbuffer = make_region_pixelbuffer_from_resource(
            ($datadir || "") + 'images/' + image[1],
            image[2].to_i, image[3].to_i, image[4].to_i, image[5].to_i)
        elsif image.is_a?(Array)
          # Fall back to sheet-style helper (first frame / region)
          pixelbuffer = tile_image_region(image, 0, 0)
        end
        
        if not hidden then
          if id == 0 or not(pixelbuffer) then
            add_tile(id, nil)
          else
            add_tile(id, Tile.new(pixelbuffer))
          end
        end

      when :tilegroup
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
