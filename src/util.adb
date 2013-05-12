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
  -- Camera position
  CameraX: Float := 0.0;
  CameraY: Float := 0.0;

  ---------------------------------------------------------------------------
  -- Projection Scale
  Projection_Scale: constant Float := 1.0;

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

    -- Save the default modelview matrix
    GL.Push_Matrix;

    -- Initialize clear color
    GL.Clear_Color (0.0, 0.0, 0.0, 1.0);

    -- Check for error
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

    -- Pop default matrix onto current matrix
    GL.Matrix_Mode (GL.GL_MODELVIEW);
    GL.Pop_Matrix;

    -- Save default matrix again
    GL.Push_Matrix;

    -- Move to center of the screen
    GL.Translate
      ( Float (Screen_Width   / 2) * Projection_Scale,
        Float (Screen_Height  / 2) * Projection_Scale,
        0.0
      );

    -- Red quad
    GL.Begin_Primitive (GL.GL_QUADS);
    begin
      GL.Color  (Float (1.0), 0.0, 0.0);
      GL.Vertex ( -Float (Screen_Width) / 4.0, -Float (Screen_Height) / 4.0);
      GL.Vertex (  Float (Screen_Width) / 4.0, -Float (Screen_Height) / 4.0);
      GL.Vertex (  Float (Screen_Width) / 4.0,  Float (Screen_Height) / 4.0);
      GL.Vertex ( -Float (Screen_Width) / 4.0,  Float (Screen_Height) / 4.0);
    end;
    GL.End_Primitive;

    -- Move to the right of the screen
    GL.Translate (Float (Screen_Width), 0.0, 0.0);

    -- Green quad
    GL.Begin_Primitive (GL.GL_QUADS);
    begin
      GL.Color  (Float (0.0), 1.0, 0.0);
      GL.Vertex ( -Float (Screen_Width) / 4.0, -Float (Screen_Height) / 4.0);
      GL.Vertex (  Float (Screen_Width) / 4.0, -Float (Screen_Height) / 4.0);
      GL.Vertex (  Float (Screen_Width) / 4.0,  Float (Screen_Height) / 4.0);
      GL.Vertex ( -Float (Screen_Width) / 4.0,  Float (Screen_Height) / 4.0);
    end;
    GL.End_Primitive;

    -- Move to the lower right of the screen
    GL.Translate (Float (0.0), Float (Screen_Height), 0.0);

    -- Blue quad
    GL.Begin_Primitive (GL.GL_QUADS);
    begin
      GL.Color  (Float (0.0), 0.0, 1.0);
      GL.Vertex ( -Float (Screen_Width) / 4.0, -Float (Screen_Height) / 4.0);
      GL.Vertex (  Float (Screen_Width) / 4.0, -Float (Screen_Height) / 4.0);
      GL.Vertex (  Float (Screen_Width) / 4.0,  Float (Screen_Height) / 4.0);
      GL.Vertex ( -Float (Screen_Width) / 4.0,  Float (Screen_Height) / 4.0);
    end;
    GL.End_Primitive;

    -- Move below the screen
    GL.Translate (-Float (Screen_Width), 0.0, 0.0);

    -- Yellow quad
    GL.Begin_Primitive (GL.GL_QUADS);
    begin
      GL.Color  (Float (1.0), 1.0, 0.0);
      GL.Vertex ( -Float (Screen_Width) / 4.0, -Float (Screen_Height) / 4.0);
      GL.Vertex (  Float (Screen_Width) / 4.0, -Float (Screen_Height) / 4.0);
      GL.Vertex (  Float (Screen_Width) / 4.0,  Float (Screen_Height) / 4.0);
      GL.Vertex ( -Float (Screen_Width) / 4.0,  Float (Screen_Height) / 4.0);
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
  begin
    if Events.To_Character (Symbol) = 'w' then
      CameraY := CameraY - 16.0;
    elsif Events.To_Character (Symbol) = 's' then
      CameraY := CameraY + 16.0;
    elsif Events.To_Character (Symbol) = 'a' then
      CameraX := CameraX - 16.0;
    elsif Events.To_Character (Symbol) = 'd' then
      CameraX := CameraX + 16.0;
    elsif Events.To_Character (Symbol) = ASCII.ESC then
      Terminated := True;
    end if;

    -- Take saved matrix off the stack and reset it
    GL.Matrix_Mode (GL.GL_MODELVIEW);
    GL.Pop_Matrix;
    GL.Load_Identity;

    -- Move camera to position
    GL.Translate (-CameraX, -CameraY, 0.0);

    -- Save default matrix again with camera translation
    GL.Push_Matrix;
  end Key_Press;

  ---------------------------------------------------------------------------

end Util;
