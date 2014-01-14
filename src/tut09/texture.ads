pragma License (GPL);
------------------------------------------------------------------------------
-- EMAIL: <darkestkhan@gmail.com>                                           --
-- License: GNU GPLv3 or any later as published by Free Software Foundation --
-- (see README file)                                                        --
--                    Copyright © 2014 darkestkhan                          --
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

with Rect;
package Texture is

  ---------------------------------------------------------------------------

  type Texture is private;
  type UInt_Array is array (Natural range <>) of GL.Uint;
  type UInt_Array_Access is access UInt_Array;

  ---------------------------------------------------------------------------

  procedure Load_Texture_From_File
    ( This: in out Texture; Path: in String; Success: out Boolean
    );

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
    ( This      : in  out Texture;
      Pixels    : in      GL.Pointer;
      Img_Width : in      GL.UInt;
      Img_Height: in      GL.UInt;
      Tex_Width : in      GL.UInt;
      Tex_Height: in      GL.UInt;
      Success   :     out Boolean
    );

  ---------------------------------------------------------------------------
  -- Pre:
  --    A valid OpenGL context
  --    Active modelview matrix
  -- Post:
  --    Translates to given position and renders
  --      the texture area mapped to a quad
  --    If given texture clip is NULL, the full texture is rendered
  -- Side:
  --    Binds member texture ID
  procedure Render
    ( This: in  out Texture;
      X   : in      Float;
      Y   : in      Float;
      Do_Clip: in   Boolean;
      Clip: in      Rect.Rect_F := Rect.Rect_F'(0.0, 0.0, 0.0, 0.0)
    );

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

  ---------------------------------------------------------------------------
  -- Pre:
  --    None
  -- Post:
  --    Returns image height
  -- Side:
  --    None
  function Img_Width (This: in Texture) return GL.UInt;

  ---------------------------------------------------------------------------
  -- Pre:
  --    None
  -- Post:
  --    Returns image height
  -- Side:
  --    None
  function Img_Height (This: in Texture) return GL.UInt;

  ---------------------------------------------------------------------------
  -- Pre:
  --    An existing unlocked texture
  -- Post:
  --    Gets pixels from texture data
  --    Returrns True if pixels where retrieved
  -- Side:
  --    Binds a Null texture
  procedure Lock (This: in out Texture; Success: out Boolean);

  ---------------------------------------------------------------------------
  -- Pre:
  --    A locked texture
  -- Post:
  --    Updates texture with pixels
  --    Returns True if if texture pixels where retrieved
  -- Side:
  --    Binds a Null texture
  procedure Unlock (This: in out Texture; Success: out Boolean);

  ---------------------------------------------------------------------------
  -- Pre:
  --    Available pixels
  -- Post:
  --    Returns pixels
  -- Side:
  --    None
  function Get_Pixel_Data_32 (This: in Texture) return UInt_Array_Access;

  ---------------------------------------------------------------------------
  -- Pre:
  --    Available pixels
  -- Post:
  --    Returns pixel
  -- Side:
  --    None
  function Get_Pixel_32
    ( This: in Texture;
      X   : in GL.UInt;
      Y   : in GL.UInt
    ) return GL.UInt;

  ---------------------------------------------------------------------------
  -- Pre:
  --    Available member pixels
  -- Post:
  --    Sets pixels at given position
  --    Function will segfault if texture is not locked
  -- Side:
  --    None
  procedure Set_Pixel_32
    ( This  : in out  Texture;
      X     : in      GL.UInt;
      Y     : in      GL.UInt;
      Pixel : in      GL.UInt
    );

  ---------------------------------------------------------------------------

private

  ---------------------------------------------------------------------------


  type Texture is
  record
    ID        : aliased GL.UInt   := 0;  -- Texture ID/name
    Width     : aliased GL.UInt   := 0;  -- Texture width
    Height    : aliased GL.UInt   := 0;  -- Texture height
    Img_Width : aliased GL.UInt   := 0;  -- Image width
    Img_Height: aliased GL.UInt   := 0;  -- Image height
    Pixels    : UInt_Array_Access := Null;  -- Pixels in the image
  end record;

  ---------------------------------------------------------------------------

end Texture;
