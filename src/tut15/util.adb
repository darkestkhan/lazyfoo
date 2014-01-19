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
package body Util is

  ---------------------------------------------------------------------------
  -- Utility renames
  package TIO renames Ada.Text_IO;

  ---------------------------------------------------------------------------
  -- Textures
  Loaded_Texture: Texture.Texture;
  -- Texture offset
  Tex_X: Float := 0.0;
  Tex_Y: Float := 0.0;
  Wrap_Type: GL.Int := 0;

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

    -- Set blending
    GL.Enable (GL.GL_BLEND);
    GL.Disable (GL.GL_DEPTH_TEST);
    GL.Blend_Func (GL.GL_SRC_ALPHA, GL.GL_ONE_MINUS_SRC_ALPHA);

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
    -- Load and colour key texture
    Texture.Load_Texture_From_File_With_Colour_Key
      ( Loaded_Texture, "data/12.png", 16#FF_FF_FF_FF#, Success );

    if not Success then
      TIO.Put_Line (TIO.Standard_Error, "Unable to load texture");
      return False;
    else
      return True;
    end if;
  end Load_Media;

  ---------------------------------------------------------------------------

  procedure Update is
  begin
    -- Scroll texture
    Tex_X := Tex_X + 1.0;
    Tex_Y := Tex_Y + 1.0;

    -- Cap scrolling
    if Tex_X >= Float (Texture.Width (Loaded_Texture)) then
      Tex_X := 0.0;
    end if;

    if Tex_Y >= Float (Texture.Height (Loaded_Texture)) then
      Tex_Y := 0.0;
    end if;
  end Update;

  ---------------------------------------------------------------------------

  procedure Render (Win: in Window.Window_Handle)
  is
    use type GL.UInt;

    -- Calculate texture maxima
    Texture_Right : constant Float :=
      Float (Screen_Width) / Float (Texture.Width (Loaded_Texture));
    Texture_Bottom: constant Float :=
      Float (Screen_Height) / Float (Texture.Height (Loaded_Texture));
  begin
    -- Clear color buffer
    GL.Clear (GL.GL_COLOR_BUFFER_BIT);

    -- Use texture
    GL.Bind_Texture (GL.GL_TEXTURE_2D, Texture.ID (Loaded_Texture));

    -- Switch to txture matrix
    GL.Matrix_Mode (GL.GL_TEXTURE);

    -- Reset transformation
    GL.Load_Identity;

    -- Scroll texture
    GL.Translate
      ( Tex_X / Float (Texture.Width (Loaded_Texture)),
        Tex_Y / Float (Texture.Height (Loaded_Texture)),
        0.0
      );

    -- Render
    GL.Begin_Primitive (GL.GL_QUADS);
    begin
      GL.Tex_Coord (Float (0.0), 0.0);
      GL.Vertex (Float (0.0), 0.0);
      GL.Tex_Coord (Texture_Right, 0.0);
      GL.Vertex (Float (Screen_Width), 0.0);
      GL.Tex_Coord (Texture_Right, Texture_Bottom);
      GL.Vertex (Float (Screen_Width), Float (Screen_Height));
      GL.Tex_Coord (0.0, Texture_Bottom);
      GL.Vertex (0.0, Float (Screen_Height));
    end;
    GL.End_Primitive;

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

    use type GL.UInt;
  begin
    case Events.To_Character (Symbol) is
      when ASCII.ESC  => Terminated := True;
      when 'q'        =>
        -- Cycle through texture repetitions
        Wrap_Type := Wrap_Type + 1;
        if Wrap_Type > 4 then
          Wrap_Type := 0;
        end if;

        -- Set texture repetition
        GL.Bind_Texture (GL.GL_TEXTURE_2D, Texture.ID (Loaded_Texture));

        case Wrap_Type is
          when 0 =>
            GL.Tex_Parameter
              ( GL.GL_TEXTURE_2D,
                GL.GL_TEXTURE_WRAP_S,
                GL.GL_REPEAT
              );
            GL.Tex_Parameter
              ( GL.GL_TEXTURE_2D,
                GL.GL_TEXTURE_WRAP_T,
                GL.GL_REPEAT
              );
          when 1 =>
            GL.Tex_Parameter
              ( GL.GL_TEXTURE_2D,
                GL.GL_TEXTURE_WRAP_S,
                GL.GL_CLAMP
              );
            GL.Tex_Parameter
              ( GL.GL_TEXTURE_2D,
                GL.GL_TEXTURE_WRAP_T,
                GL.GL_CLAMP
              );
          when 2 =>
            GL.Tex_Parameter
              ( GL.GL_TEXTURE_2D,
                GL.GL_TEXTURE_WRAP_S,
                GL.GL_CLAMP_TO_BORDER
              );
            GL.Tex_Parameter
              ( GL.GL_TEXTURE_2D,
                GL.GL_TEXTURE_WRAP_T,
                GL.GL_CLAMP_TO_BORDER
              );
          when 3 =>
            GL.Tex_Parameter
              ( GL.GL_TEXTURE_2D,
                GL.GL_TEXTURE_WRAP_S,
                GL.GL_CLAMP_TO_EDGE
              );
            GL.Tex_Parameter
              ( GL.GL_TEXTURE_2D,
                GL.GL_TEXTURE_WRAP_T,
                GL.GL_CLAMP_TO_EDGE
              );
          when 4 =>
            GL.Tex_Parameter
              ( GL.GL_TEXTURE_2D,
                GL.GL_TEXTURE_WRAP_S,
                GL.Enum (16#8370#)
              );
            GL.Tex_Parameter
              ( GL.GL_TEXTURE_2D,
                GL.GL_TEXTURE_WRAP_T,
                GL.Enum (16#8370#)
              );
          when others =>
            Null;
        end case;
      when others     => Null;
    end case;
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
