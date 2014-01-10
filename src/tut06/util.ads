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

  Win: Lumen.Window.Window_Handle;

  ---------------------------------------------------------------------------
  -- Screen constants
  Screen_Width  : constant Natural := 640;
  Screen_Height : constant Natural := 480;
  Screen_FPS    : constant Natural := 60;

  ---------------------------------------------------------------------------
  -- Global holding key information whether or not program is still running
  Terminated: Boolean := False;

  ---------------------------------------------------------------------------
  -- Pre:
  --    Valid OpenGL context
  -- Post:
  --    Initialized matrices and clear color
  --    Report to console if there was an OpenGL error
  --    Returns False if there was an error in initialization
  -- Side:
  --    Sets viewport to the fill rendering data
  --    Projection matrix is set to an orthographic matrix
  --    Modelview matrix is set to identity matrix
  --    Matrix mode is set to modelview
  --    Clear color is set to black
  --    Texturing is enabled
  function Init_GL return Boolean; 

  ---------------------------------------------------------------------------
  -- Pre:
  --    A valid OpenGL context
  -- Post:
  --    Loads media to use in the program
  --    Reports to the console if there was problem in loading the media
  --    Returns True of the media loaded succesfully
  -- Side:
  --    None
  -- NOTE:
  --    Associated Unload_Media function should be also written and called when
  --      closing application
  function Load_Media return Boolean;

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

  function New_Frame (Frame_Delta: in Duration) return Boolean;

  ---------------------------------------------------------------------------

end Util;
