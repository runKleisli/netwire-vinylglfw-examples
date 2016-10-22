# netwire-vinylglfw-examples

Port of netwire-input-glfw example to VinylGL & GLSL 1.50. Uses Netwire 5 and Vinyl >= 0.4. NetVinylGLFW is a previous combination of Netwire, VinylGL, & GLFW, but its Netwire and Vinyl versions are outdated as of 2016. The combination with STM that it suggests is embraced by netwire-input-glfw.

Usage: Place the executable and the `etc` folder in the same directory. Change to that directory in the command line and run the executable.

## Style progression

Style 0 follows the original structure of the netwire-input-glfw example.

Style 1 shows how the input structuralization step is obviated by the shader programs taking Vinyl records as input to begin with.

Style 2 generalizes `renderWire` to all render functions of this VinylGL style, and in a way which allows the input record to have extra fields beyond those required by the shader program.

Input structuralization is done upon the data being collected from the appropriate wires, since what `renderWire` does to the render function is now independent of the input structure.

Allowing the wire to handle records with more fields just separates what data has been collected (in a record structure) from what it's being used for in this case, which is to control the shader program. Taken to an extreme, one could manage a global application state record, as in the VinylGL examples, and render from the data in that record without having to change any types or plumbing to account for the extra fields the record has (or gets as development goes on).
