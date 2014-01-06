pragma License (GPL);
------------------------------------------------------------------------------
-- EMAIL: <darkestkhan@gmail.com>                                           --
-- License: GNU GPLv3 or any later as published by Free Software Foundation --
-- (see README file)                                                        --
--                    Copyright © 2013 darkestkhan                          --
------------------------------------------------------------------------------
--  This Program is Free Software: You can redistribute it and/or modify    --
--  it under the terms of The GNU General Public License as published by    --
--    the Free Software Foundation, either version 3 of the license, or     --
--                (at Your option) any later version.                       --
--                                                                          --
--      This Program is distributed in the hope that it will be useful,     --
--      but WITHOUT ANY WARRANTY; without even the implied warranty of      --
--      MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE. See the        --
--              GNU General Public License for more details.                --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--   along with this program. If not, see <http://www.gnu.org/licenses/>.   --
------------------------------------------------------------------------------
with Lumen; use Lumen;
with Lumen.GL;
package Texture is

  type Texture is private;
  type UInt_Array is array (Natural range <>) of GL.Uint;

  ---------------------------------------------------------------------------
  -- Pre:
  --    A valid OpenGL context
  -- Post:
  --    Deletes texture if it exists
  --    Sets texture ID to 0
  -- Side:
  --    None
  procedure Delete_Texture (This: in out Texture);

  ---------------------------------------------------------------------------
  -- Pre:
  --    A valid OpenGL context
  -- Post:
  --    Creates a texture from the given pixels
  --    Reports errors to console if texture couldn't be created
  --    Success is False if there was an error in loading texture
  --      and tru if there were no errors in loading texture
  -- Side:
  --    Binds a Null texture
  procedure Load_From_Pixels_32
    ( This: in out Texture; Pixels: in GL.Pointer;
      Width: in GL.Uint; Height: in GL.Uint; Success: out Boolean
    );

  ---------------------------------------------------------------------------
  -- Pre:
  --    A valid OpenGL context
  --    Active modelview matrix
  -- Post:
  --    Translates to given position and renders texture quad
  -- Side:
  --    Binds texture ID
  procedure Render (This: in Texture; X: in Float; Y: in Float);

  ---------------------------------------------------------------------------
  -- Pre:
  --    None
  -- Post:
  --    Returns texture name/ID
  -- Side:
  --    None
  function ID (This: in Texture) return GL.UInt;

  ---------------------------------------------------------------------------
  -- Pre:
  --    None
  -- Post:
  --    Returns texture width
  -- Side:
  --    None
  function Width (This: in Texture) return GL.UInt;

  ---------------------------------------------------------------------------
  -- Pre:
  --    None
  -- Post:
  --    Returns texture height
  -- Side:
  --    None
  function Height (This: in Texture) return GL.UInt;

private

  type Texture is
  record
    ID    : aliased GL.UInt := 0;  -- Texture ID/name
    Width : aliased GL.UInt := 0;  -- Texture width
    Height: aliased GL.UInt := 0;  -- Texture height
  end record;

end Texture;
