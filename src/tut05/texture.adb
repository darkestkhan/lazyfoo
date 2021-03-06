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
with Ada.Text_IO;

package body Texture is

  ---------------------------------------------------------------------------
  -- Utility renames
  package TIO renames Ada.Text_IO;

  ---------------------------------------------------------------------------

  procedure Load_From_Pixels_32
    ( This: in out Texture; Pixels: in GL.Pointer;
      Width: in GL.Uint; Height: in GL.Uint; Success: out Boolean
    )
  is
  begin
    -- Free texture if it exists
    Delete_Texture (This);

    -- Get texture dimensions
    This.Width  := Width;
    This.Height := Height;

    -- Generate texture ID
    GL.Gen_Textures (1, This.ID'Address);

    -- Bind texture ID
    GL.Bind_Texture (GL.GL_TEXTURE_2D, This.ID);

    -- Generate texture
    GL.Tex_Image
      ( GL.GL_TEXTURE_2D, 0, GL.GL_RGBA, GL.Int (Width), GL.Int (Height), 0,
        GL.GL_RGBA, GL.GL_UNSIGNED_BYTE, Pixels
      );

    -- Set texture parameters
    GL.Tex_Parameter (GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);
    GL.Tex_Parameter (GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR);

    -- Unbind texture
    GL.Bind_Texture (GL.GL_TEXTURE_2D, 0);

    -- Check for error
    declare
      use type GL.Enum;
      Error: constant GL.Enum := GL.Get_Error;
    begin
      if Error /= GL.GL_NO_ERROR then
        TIO.Put_Line (TIO.Standard_Error, "Error initializing OpenGL!");
        Success := False;
      end if;
    end;
    Success := True;
  end Load_From_Pixels_32;

  ---------------------------------------------------------------------------

  procedure Delete_Texture (This: in out Texture)
  is
    use type GL.UInt;
  begin
    if This.ID /= 0 then
      GL.Delete_Textures (1, This.ID'Address);
      This.ID := 0;
    end if;

    This.Width  := 0;
    This.Height := 0;
  end Delete_Texture;

  ---------------------------------------------------------------------------

  procedure Render (This: in Texture; X: in Float; Y: in Float)
  is
    use type GL.UInt;
  begin
    -- If the texture exists
    if This.ID /= 0 then
      -- Remove any previous transformations
      GL.Load_Identity;

      -- Move to rendering point
      GL.Translate (X, Y, Float (0.0));

      -- Set texture ID
      GL.Bind_Texture (GL.GL_TEXTURE_2D, This.ID);

      -- Render texture quad
      GL.Begin_Primitive (GL.GL_QUADS);
      begin
        GL.Tex_Coord (Float (0.0), 0.0);
        GL.Vertex (Float (0.0), 0.0);
        GL.Tex_Coord (Float (1.0), 0.0);
        GL.Vertex (Float (This.Width), 0.0);
        GL.Tex_Coord (Float (1.0), 1.0);
        GL.Vertex (Float (This.Width), Float (This.Height));
        GL.Tex_Coord (Float (0.0), 1.0);
        GL.Vertex (Float (0.0), Float (This.Height));
      end;
      GL.End_Primitive;
    else
      TIO.Put_Line (TIO.Standard_Error, "Tried to render non-existent texture.");
    end if;
  end Render;

  ---------------------------------------------------------------------------

  function ID (This: in Texture) return GL.UInt
  is
  begin
    return This.ID;
  end ID;

  ---------------------------------------------------------------------------

  function Width (This: in Texture) return GL.UInt
  is
  begin
    return This.Width;
  end Width;

  ---------------------------------------------------------------------------

  function Height (This: in Texture) return GL.UInt
  is
  begin
    return This.Height;
  end Height;

  ---------------------------------------------------------------------------

end Texture;
