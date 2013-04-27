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
with Lumen.Window;
with Lumen.Events;
package Util is

  ---------------------------------------------------------------------------

  type Viewport_Mode is
    ( Full, Half_Center, Half_Top, Quad, Radar );

  ---------------------------------------------------------------------------
  -- Screen constants
  Screen_Width  : constant Natural := 640;
  Screen_Height : constant Natural := 480;
  Screen_FPS    : constant Natural := 60;

  ---------------------------------------------------------------------------
  -- Color modes
  Color_Mode_Cyan : constant Integer := 0;
  Color_Mode_Multi: constant Integer := 1;

  ---------------------------------------------------------------------------
  -- Global holding key information whether or not program is still running
  Terminated: Boolean := False;

  ---------------------------------------------------------------------------
  -- Pre:
  --    Valid OpenGL context
  -- Post:
  --    Initialized matrices and clear color
  --    Report to console if there was an OpenGL error
  --    Return False if there was an error in initialization
  -- Side:
  --    Projection matrix is set to identity
  --    Modelview matrix is set to identity
  --    Matrix mode is set to modelview
  --    Clear color is set to black
  function Init_GL return Boolean; 

  ---------------------------------------------------------------------------
  -- Pre:
  --    None
  -- Post:
  --    Does per frame logic
  -- Side:
  --    None
  procedure Update;

  ---------------------------------------------------------------------------
  -- Pre:
  --    A valid OpenGL context
  --    Active modelview matrix
  -- Post:
  --    Renders the scene
  -- Side:
  --    Clears the color buffer
  --    Swaps the front/back buffer
  --    Translates modelview matrix to the center of the default screen
  --    Changes the current rendering color
  procedure Render (Win: in Lumen.Window.Window_Handle);

  ---------------------------------------------------------------------------
  -- Pre:
  --    None
  -- Post:
  --    Toggles the color mode when user presses 'q'
  --    Cycles through different projection scales when the user presses 'e'
  -- Side:
  --    When the user presses 'e' the matrix mode is set to projection
  procedure Key_Press
    ( Category  : Lumen.Events.Key_Category;
      Symbol    : Lumen.Events.Key_Symbol;
      Modifiers : Lumen.Events.Modifier_Set
    );

  ---------------------------------------------------------------------------

end Util;
