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
with Lumen; use Lumen;
with Lumen.Window;
with Lumen.Events;

package Util is

  ---------------------------------------------------------------------------
  -- Screen constants
  Screen_Width  : constant Integer := 640;
  Screen_Height : constant Integer := 480;
  Screen_FPS    : constant Integer := 60;

  ---------------------------------------------------------------------------
  -- Global holding information whether or not program is still runnning
  Terminated: Boolean := False;

  ---------------------------------------------------------------------------
  -- Initializes all the OpenGL variables/states
  function Init_GL return Boolean;

  ---------------------------------------------------------------------------

  procedure Update;

  ---------------------------------------------------------------------------
  -- Render the scene
  procedure Render (Win: in Lumen.Window.Window_Handle);

  ---------------------------------------------------------------------------

  procedure Key_Press
    ( Category  : Lumen.Events.Key_Category;
      Symbol    : Lumen.Events.Key_Symbol;
      Modifiers : Lumen.Events.Modifier_Set
    );

  ---------------------------------------------------------------------------

end Util;
