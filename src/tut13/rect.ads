pragma License (GPL);
------------------------------------------------------------------------------
-- EMAIL: <darkestkhan@gmail.com>                                           --
-- License: GNU GPLv3 or any later as published by Free Software Foundation --
-- (see COPYING file)                                                       --
--                                                                          --
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

package Rect is

  ---------------------------------------------------------------------------

  type Rect_F is
  record
    X: Float := 0.0;
    Y: Float := 0.0;
    W: Float := 0.0;
    H: Float := 0.0;
    Perform: Boolean := False;
  end record;

  type Rect_F_Access is access Rect_F;
  type Rect_F_Array  is array (Natural range <>) of Rect_F;

  ---------------------------------------------------------------------------

  Null_Rect_F: constant Rect_F := Rect_F'(0.0, 0.0, 0.0, 0.0, False);

  ---------------------------------------------------------------------------

end Rect;
