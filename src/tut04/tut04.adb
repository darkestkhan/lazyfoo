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

with Util;
with Lumen; use Lumen;
with Lumen.Window;
with Lumen.Events.Animate;
procedure Tut04 is

  ---------------------------------------------------------------------------

  package TIO renames Ada.Text_IO;

  ---------------------------------------------------------------------------

  Win: Window.Window_Handle;

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

begin
  Window.Create
    ( Win, Width => Util.Screen_Width,
      Height => Util.Screen_Height, Name => "OpenGL"
    );

  Win.Key_Press := Util.Key_Press'Unrestricted_Access;

  if not Util.Init_GL then
    TIO.Put_Line ("Unable to initialize graphics library!");
    raise Program_Error;
  end if;
  Events.Animate.Run (Win, 60, New_Frame'Unrestricted_Access);

end Tut04;
