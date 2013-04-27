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

with Lumen; use Lumen;
with Lumen.GL;
package body Util is

  ---------------------------------------------------------------------------
  -- Utility renames
  package TIO renames Ada.Text_IO;

  ---------------------------------------------------------------------------
  -- Current color rendering mode
  Color_Mode: Integer := Color_Mode_Cyan;

  -- Projection Scale
  Projection_Scale: Float := 1.0;

  -- Viewport mode
  Viewport: Viewport_Mode := Full;

  ---------------------------------------------------------------------------


  function Init_GL return Boolean is
    use type GL.Enum;
  begin
    -- Set the viewport
    GL.Viewport (0.0, 0.0, Float (Screen_Width), Float (Screen_Height));

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

    declare
      Error: constant GL.Enum := GL.Get_Error;
    begin
      if Error /= GL.GL_NO_ERROR then
        TIO.Put_Line ("Error initializing OpenGL!");
        return False;
      end if;
    end;
    return True;
  end Init_GL;

  ---------------------------------------------------------------------------

  procedure Update is
  begin
    Null;
  end Update;

  ---------------------------------------------------------------------------

  procedure Render (Win: in Window.Window_Handle) is
  begin
    -- Clear color buffer
    GL.Clear (GL.GL_COLOR_BUFFER_BIT);

    -- Reset modelview matrix
    GL.Matrix_Mode (GL.GL_MODELVIEW);
    GL.Load_Identity;

    -- Move to center of the screen
    GL.Translate
      ( Float (Screen_Width   / 2) * Projection_Scale,
        Float (Screen_Height  / 2) * Projection_Scale,
        0.0
      );

    -- Render quad
    if (Color_Mode = Color_Mode_Cyan) then
      GL.Begin_Primitive (GL.GL_QUADS);
      begin
        GL.Color  (Float (  0.0),   1.0,   1.0);
        GL.Vertex (Float (-50.0), -50.0);
        GL.Vertex (Float ( 50.0), -50.0);
        GL.Vertex (Float ( 50.0),  50.0);
        GL.Vertex (Float (-50.0),  50.0);
      end;
      GL.End_Primitive;
    else
      -- RYGB Mix
      GL.Begin_Primitive (GL.GL_QUADS);
      begin
        GL.Color (Float (1.0), 0.0, 0.0); GL.Vertex (Float (-50.0), -50.0);
        GL.Color (Float (1.0), 1.0, 0.0); GL.Vertex (Float ( 50.0), -50.0);
        GL.Color (Float (0.0), 1.0, 0.0); GL.Vertex (Float ( 50.0),  50.0);
        GL.Color (Float (0.0), 0.0, 1.0); GL.Vertex (Float (-50.0),  50.0);
      end;
      GL.End_Primitive;
    end if;

    Window.Swap (Win);
  end Render;

  ---------------------------------------------------------------------------

  procedure Key_Press
    ( Category  : Events.Key_Category;
      Symbol    : Events.Key_Symbol;
      Modifiers : Events.Modifier_Set
    ) is
  begin
    if Events.To_Character (Symbol) = 'q' then
      if Color_Mode = Color_Mode_Cyan then
        Color_Mode := Color_Mode_Multi;
      else
        Color_Mode := Color_Mode_Cyan;
      end if;
    elsif Events.To_Character (Symbol) = 'e' then
      if (Projection_Scale = 1.0) then
        -- Zoom out
        Projection_Scale := 2.0;
      elsif Projection_Scale = 2.0 then
        -- Zoom in
        Projection_Scale := 0.5;
      else
        -- Regular zoom
        Projection_Scale := 1.0;
      end if;

      -- Update projection matrix
      GL.Matrix_Mode (GL.GL_PROJECTION);
      GL.Load_Identity;
      GL.Ortho
        ( 0.0, GL.Double (Float (Screen_Width) * Projection_Scale),
          GL.Double (Float (Screen_Height) * Projection_Scale), 0.0, 1.0, -1.0
        );
    end if;
    Terminated := Events.To_Character (Symbol) = ASCII.ESC;
  end Key_Press;

  ---------------------------------------------------------------------------

end Util;
