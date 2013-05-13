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

with Lumen.GL;
package body Util is

  ---------------------------------------------------------------------------
  -- Convenience renames
  package TIO renames Ada.Text_IO;

  ---------------------------------------------------------------------------

  function Init_GL return Boolean
  is
  begin
    -- Initialize projection matrix
    GL.Matrix_Mode (GL.GL_PROJECTION);
    GL.Load_Identity;

    -- Initialize modelview matrix
    GL.Matrix_Mode (GL.GL_MODELVIEW);
    GL.Load_Identity;

    -- Initialize clear color
    GL.Clear_Color (Float (0.0), 0.0, 0.0, 0.0);

    -- Check for error
    declare
      use type GL.Enum;
      Error: constant GL.Enum := GL.Get_Error;
    begin
      if (Error /= GL.GL_NO_ERROR) then
        TIO.Put_Line ("Error Initializing OpenGL!");
        return False;
      end if;
    end;
    return True;
  end Init_GL;

  ---------------------------------------------------------------------------

  procedure Update is
  begin
    null;
  end Update;

  ---------------------------------------------------------------------------

  procedure Render (Win: in Lumen.Window.Window_Handle)
  is
  begin
    -- Clear color buffer
    GL.Clear (GL.GL_COLOR_BUFFER_BIT);

    -- Render quad
    GL.Begin_Primitive (GL.GL_QUADS);
    begin
      GL.Vertex (-Float (0.5), -0.5);
      GL.Vertex ( Float (0.5), -0.5);
      GL.Vertex ( Float (0.5),  0.5);
      GL.Vertex (-Float (0.5),  0.5);
    end;
    GL.End_Primitive;

    Window.Swap (Win);
  end Render;

  ---------------------------------------------------------------------------

  procedure Key_Press
    ( Category  : Lumen.Events.Key_Category;
      Symbol    : Lumen.Events.Key_Symbol;
      Modifiers : Lumen.Events.Modifier_Set
    )
  is
    pragma Unreferenced (Category);
    pragma Unreferenced (Modifiers);
  begin
    Terminated := Events.To_Character (Symbol) = ASCII.ESC;
  end Key_Press;

  ---------------------------------------------------------------------------
end Util;
