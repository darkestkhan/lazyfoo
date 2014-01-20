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
with System;

with Ada.Text_IO;

with Lumen; use Lumen;
with Lumen.GL;

with Imago; use Imago;
with Imago.IL;
with Imago.ILU;

package body Util is

  ---------------------------------------------------------------------------
  -- Utility renames
  package TIO renames Ada.Text_IO;

  ---------------------------------------------------------------------------
  -- Vertices
  Quad_Vertices: array (0 .. 7) of Float;
  -- Vertex indices
  Indices: array (0 .. 3) of GL.UInt;
  -- Vertex buffer
  Vertex_Buffer: GL.UInt := 0;
  -- Index buffer
  Index_Buffer: GL.UInt := 0;

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

    -- Initialize projection matrix
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
  begin
    -- Set quad vertices
    Quad_Vertices (0) := Float (Screen_Width) * 1.0 / 4.0;
    Quad_Vertices (1) := Float (Screen_Height) * 1.0 / 4.0;

    Quad_Vertices (2) := Float (Screen_Width) * 3.0 / 4.0;
    Quad_Vertices (3) := Float (Screen_Height) * 1.0 / 4.0;

    Quad_Vertices (4) := Float (Screen_Width) * 3.0 / 4.0;
    Quad_Vertices (5) := Float (Screen_Height) * 3.0 / 4.0;

    Quad_Vertices (6) := Float (Screen_Width) * 1.0 / 4.0;
    Quad_Vertices (7) := Float (Screen_Height) * 3.0 / 4.0;

    -- Set rendering indices
    for K in Indices'Range loop
      Indices (K) := GL.UInt (K);
    end loop;

    -- Create VBO
    GL.Gen_Buffers (1, Vertex_Buffer'Address);
    GL.Bind_Buffer (GL.GL_ARRAY_BUFFER, Vertex_Buffer);
    GL.Buffer_Data
      ( GL.GL_ARRAY_BUFFER,
        Quad_Vertices'Size * (Float'Size / 8),
        Quad_Vertices'Address,
        GL.GL_STATIC_DRAW
      );

    -- Create IBO
    GL.Gen_Buffers (1, Index_Buffer'Address);
    GL.Bind_Buffer (GL.GL_ELEMENT_ARRAY_BUFFER, Index_Buffer);
    GL.Buffer_Data
      ( GL.GL_ELEMENT_ARRAY_BUFFER,
        Indices'Size * (GL.UInt'Size / 8),
        Indices'Address,
        GL.GL_STATIC_DRAW
      );

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
  begin
    -- Clear color buffer
    GL.Clear (GL.GL_COLOR_BUFFER_BIT);

    -- Enable vertex arrays
    GL.Enable_Client_State (GL.GL_VERTEX_ARRAY);
    begin
      -- Set vertex data
      GL.Bind_Buffer (GL.GL_ARRAY_BUFFER, Vertex_Buffer);
      GL.Vertex_Pointer (2, GL.GL_FLOAT, 0, System'To_Address (0));
      -- Draw quad using vertex and index data
      GL.Bind_Buffer (GL.GL_ELEMENT_ARRAY_BUFFER, Index_Buffer);
      GL.Draw_Elements
        ( GL.GL_QUADS,
          4,
          GL.GL_UNSIGNED_INT,
          System'To_Address (0)
        );
    end;
    -- Disable vertex arrays
    GL.Disable_Client_State (GL.GL_VERTEX_ARRAY);

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
