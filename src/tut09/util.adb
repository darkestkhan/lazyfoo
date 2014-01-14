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
with Ada.Text_IO;

with Lumen; use Lumen;
with Lumen.GL;

with Imago; use Imago;
with Imago.IL;
with Imago.ILU;

with Texture;
with Rect;
package body Util is

  ---------------------------------------------------------------------------
  -- Utility renames
  package TIO renames Ada.Text_IO;

  ---------------------------------------------------------------------------
  -- Textures
  Loaded_Texture: Texture.Texture;

  ---------------------------------------------------------------------------

  function Init_GL return Boolean is
    use type GL.Enum;
  begin
    -- Set the viewport
    GL.Viewport (0, 0, Screen_Width, Screen_Height);

    -- Initialize Projection matrix
    GL.Matrix_Mode (GL.GL_PROJECTION);
    GL.Load_Identity;
    GL.Ortho
      ( 0.0, GL.Double (Screen_Width), GL.Double (Screen_Height),
        0.0, 1.0, -1.0
      );

    -- Initialize Modelview matrix
    GL.Matrix_Mode (GL.GL_MODELVIEW);
    GL.Load_Identity;

    -- Initialize clear color
    GL.Clear_Color (0.0, 0.0, 0.0, 1.0);

    -- Enable texturing
    GL.Enable (GL.GL_TEXTURE_2D);

    -- Check for error
    declare
      Error: constant GL.Enum := GL.Get_Error;
    begin
      if Error /= GL.GL_NO_ERROR then
        TIO.Put_Line (TIO.Standard_Error, "Error initializing OpenGL!");
        return False;
      end if;
    end;

    -- Initialize DevIL
    IL.Init;
    ILU.Init;
    IL.Clear_Color (1.0, 1.0, 1.0, 0.0);
    
    declare
      use type IL.Enum;
      Error: constant IL.Enum := IL.Get_Error;
    begin
      if Error /= IL.IL_NO_ERROR then
        TIO.Put_Line
          ( TIO.Standard_Error,
            "Error initializing DevIL! " & ILU.Error_String (Error)
          );
        return False;
      end if;
    end;

    return True;
  end Init_GL;

  ---------------------------------------------------------------------------

  function Load_Media return Boolean
  is
    use type GL.UInt;

    Success: Boolean := False;
  begin
    -- Load texture
    Texture.Load_Texture_From_File (Loaded_Texture, "data/09.png", Success);

    if not Success then
      TIO.Put_Line (TIO.Standard_Error, "Unable to load texture");
      return False;
    end if;

    Texture.Lock (Loaded_Texture, Success);

    if not Success then
      TIO.Put_Line (TIO.Standard_Error, "Unable to lock texture");
      return False;
    end if;


    -- Calculate target colour
    -- Slightly better solution than the one provided by lazyfoo as it prevents
    -- artifacts from getting created when using texture that is supplied here.
    declare
      Target_Colour: constant GL.UInt := 16#00_FF_FF_00#;
      Pixels: constant Texture.UInt_Array_Access :=
        Texture.Get_Pixel_Data_32 (Loaded_Texture);
    begin
      for K in Pixels'Range loop
        if (Pixels.all (K) and Target_Colour) /= 0 then
          Pixels.all (K) := 0;
        end if;
      end loop;
    end;

    -- Diagonal lines
    for Y in 0 .. Texture.Height (Loaded_Texture) - 1 loop
      for X in 0 .. Texture.Width (Loaded_Texture) - 1 loop
        if Y mod 10 /= X mod 10 then
          Texture.Set_Pixel_32 (Loaded_Texture, X, Y, 0);
        end if;
      end loop;
    end loop;

    -- Update texture
    Texture.Unlock (Loaded_Texture, Success);

    if not Success then
      TIO.Put_Line (TIO.Standard_Error, "Unable to unlock texture");
      return False;
    end if;

    return True;
  end Load_Media;

  ---------------------------------------------------------------------------

  procedure Update is
  begin
    Null;
  end Update;

  ---------------------------------------------------------------------------

  procedure Render (Win: in Window.Window_Handle)
  is
    use type GL.UInt;

  begin
    -- Clear color buffer
    GL.Clear (GL.GL_COLOR_BUFFER_BIT);

    -- Render texture
    Texture.Render
      ( Loaded_Texture,
        Float (GL.UInt (Screen_Width)-Texture.Img_Width (Loaded_Texture))/2.0,
        Float (GL.UInt (Screen_Height)-Texture.Img_Height (Loaded_Texture))/2.0,
        False,
        Rect.Null_Clip_F
      );

    -- Update the screen
    Window.Swap (Win);
  end Render;

  ---------------------------------------------------------------------------

  procedure Key_Press
    ( Category  : Events.Key_Category;
      Symbol    : Events.Key_Symbol;
      Modifiers : Events.Modifier_Set
    )
  is
    Pragma Unreferenced (Category);
    Pragma Unreferenced (Modifiers);
  begin
    if Events.To_Character (Symbol) = ASCII.ESC then
      Terminated := True;
    end if;
  end Key_Press;

  ---------------------------------------------------------------------------

  function New_Frame (Frame_Delta: in Duration) return Boolean
  is
    pragma Unreferenced (Frame_Delta);
  begin
    Util.Update;
    Util.Render (Win);
    return not Util.Terminated;
  end New_Frame;

  ---------------------------------------------------------------------------

end Util;
