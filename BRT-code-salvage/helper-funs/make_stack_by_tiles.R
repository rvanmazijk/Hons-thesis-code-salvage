make_stack_by_tiles <- function(tiles, dir) {
    tile_stack <- raster::stack()
    for (file in tiles) {
        print(file)
        layer <- raster::raster(paste0(dir, file))
        tile_stack <- raster::stack(tile_stack, layer)
    }
    return(tile_stack)
}
