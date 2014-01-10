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
with Lumen.Window;
with Lumen.Events.Animate;

with Util;
procedure Tut06 is

  ---------------------------------------------------------------------------

  package TIO renames Ada.Text_IO;

  ---------------------------------------------------------------------------

begin
  Window.Create
    ( Util.Win,
      Width => Util.Screen_Width,
      Height => Util.Screen_Height,
      Name => "OpenGL"
    );

  Util.Win.Key_Press := Util.Key_Press'Access;

  if not Util.Init_GL then
    TIO.Put_Line (TIO.Standard_Error, "Unable to initialize graphics library!");
    raise Program_Error;
  end if;

  -- Load media
  if not Util.Load_Media then
    TIO.Put_Line (TIO.Standard_Error, "Unable to load media!");
    raise Program_Error;
  end if;

  Events.Animate.Run
    ( Util.Win, Events.Animate.Frame_Count (Util.Screen_FPS),
      Util.New_Frame'Access
    );
end Tut06;
