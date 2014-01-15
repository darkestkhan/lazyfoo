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
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Imago; use Imago;
with Imago.IL;
with Imago.ILU;
package body Texture is

  ---------------------------------------------------------------------------
  -- Simple, quick and dirty binding to memcpy as I don't feel like translating
  -- it properly into Ada atm.
  procedure memcpy (Dest: in GL.Pointer; Src: in GL.Pointer; Size: in GL.SizeI)
  is
    procedure memcpy_orig
      ( Dest: in GL.Pointer;
        Src : in GL.Pointer;
        Size: in GL.SizeI
      );
    Pragma Import (StdCall, memcpy_orig, "memcpy");
  begin
    memcpy_orig (Dest, Src, Size);
  end memcpy;

  ---------------------------------------------------------------------------
  -- Utility renames
  package TIO renames Ada.Text_IO;

  ---------------------------------------------------------------------------

  procedure Free is new
    Ada.Unchecked_Deallocation (UInt_Array, UInt_Array_Access);

  ---------------------------------------------------------------------------
  -- Pre:
  --    None
  -- Post:
  --    Returns nearest power of two that is greatest or equal to integer
  -- Side:
  --    None
  function Power_Of_Two (Num: in GL.UInt) return GL.UInt;

  ---------------------------------------------------------------------------

  procedure Load_Pixels_From_File
    ( This    : in  out Texture;
      Path    : in      String;
      Success :     out Boolean
    )
  is
    use type IL.Bool;
    use type GL.UInt;
    -- Texture loading success
    Pixels_Loaded : Boolean := False;
    IMG_ID        : IL.UInt := 0;
    Not_Error     : IL.Bool;
  begin
    -- Deallocate texture data
    Delete_Texture (This);

    -- Generate and set current image ID
    IL.Gen_Images (1, IMG_ID'Address);
    IL.Bind_Image (IMG_ID);

    -- Load image
    Not_Error := IL.Load_Image (Path);

    -- Image loaded successfully
    if (Not_Error = IL.IL_TRUE) then
      -- Convert image to RGBA
      Not_Error := IL.Convert_Image (IL.IL_RGBA, IL.IL_UNSIGNED_BYTE);
      if (Not_Error = IL.IL_TRUE) then
        -- Initialize dimensions
        declare
          Img_Width : constant GL.UInt :=
            GL.UInt (IL.Get_Integer (IL.IL_IMAGE_WIDTH));
          Img_Height: constant GL.UInt :=
            GL.UInt (IL.Get_Integer (IL.IL_IMAGE_HEIGHT));

          -- Calculate required texture dimensions
          Tex_Width : constant GL.UInt := Power_Of_Two (Img_Width);
          Tex_Height: constant GL.UInt := Power_Of_Two (Img_Height);

          Size      : constant GL.UInt := Tex_Width * Tex_Height;
        begin
          -- Texture is of wrong size
          if (Img_Width /= Tex_Width or Img_Height /= Tex_Height) then
            -- Place image at upper left
            ILU.Image_Parameter (ILU.ILU_PLACEMENT, ILU.ILU_UPPER_LEFT);

            -- Resize image
            Not_Error :=
              ILU.Enlarge_Canvas (IL.UInt (Tex_Width), IL.UInt (Tex_Height), 1);
          end if;

          -- Allocate memory for texture data
          This.Pixels := new UInt_Array (0 .. Natural (Size) - 1);

          -- Get image dimensions
          This.Width      := Tex_Width;
          This.Height     := Tex_Height;
          This.Img_Width  := Img_Width;
          This.Img_Height := Img_Height;

          -- Copy pixels
          memcpy (This.Pixels.all'Address, IL.Get_Data, GL.SizeI (Size * 4));
          -- memcpy( mPixels, ilGetData(), size * 4 ); -- How?
          Pixels_Loaded := True;
        end;
      end if;

      -- Delete image from memory
      IL.Delete_Images (1, IMG_ID'Address);
    end if;

    -- Report error
    if not Pixels_Loaded then
      TIO.Put_Line (TIO.Standard_Error, "Unable to load: " & Path);
    end if;

    Success := Pixels_Loaded;
  end Load_Pixels_From_File;

  ---------------------------------------------------------------------------

  procedure Load_Texture_From_Pixels_32
    ( This    : in  out Texture;
      Success :     out Boolean
    )
  is
    use type GL.UInt;
  begin
    -- Loading flag
    Success := True;

    -- There are loaded pixels
    if This.ID = 0 and This.Pixels /= Null then
      -- Generate texture ID
      GL.Gen_Textures (1, This.ID'Address);

      -- Bind texture ID
      GL.Bind_Texture (GL.GL_TEXTURE_2D, This.ID);

      -- Generate texture
      GL.Tex_Image
        ( GL.GL_TEXTURE_2D,
          0,
          GL.GL_RGBA,
          GL.SizeI (This.Width),
          GL.SizeI (This.Height),
          0,
          GL.GL_RGBA,
          GL.GL_UNSIGNED_BYTE,
          This.Pixels.all'Address
        );

      -- Set texture parameters
      GL.Tex_Parameter
        ( GL.GL_TEXTURE_2D,
          GL.GL_TEXTURE_MAG_FILTER,
          GL.GL_LINEAR
        );
      GL.Tex_Parameter
        ( GL.GL_TEXTURE_2D,
          GL.GL_TEXTURE_MIN_FILTER,
          GL.GL_LINEAR
        );

      GL.Bind_Texture (GL.GL_TEXTURE_2D, 0);

      -- Check for error
      declare
        Error: constant GL.Enum := GL.Get_Error;
      begin
        if Error /= GL.GL_NO_ERROR then
          TIO.Put_Line
            ( TIO.Standard_Error,
              "Error loading texture from pixels: "
            );
          Success := False;
        else
          -- Release pixels
          Free (This.Pixels);
          This.Pixels := Null;
        end if;
      end;
    else
      -- Error
      TIO.Put_Line
        ( TIO.Standard_Error,
          "Cannot load texture from current pixels!"
        );

      -- Texture alreaedy exists
      if This.ID /= 0 then
        TIO.Put_Line (TIO.Standard_Error, "A texture is already loaded!");
      elsif This.Pixels  = Null then  -- No pixel loaded
        TIO.Put_Line (TIO.Standard_Error, "No pixels to create texture from!");
      end if;

      Success := False;
    end if;
  end Load_Texture_From_Pixels_32;

  ---------------------------------------------------------------------------

  procedure Load_Texture_From_File_With_Colour_Key
    ( This    : in  out Texture;
      Path    : in      String;
      Colour  : in      GL.UInt; -- 16#AABBGGRR#
      Success :     out Boolean
    )
  is
    use type GL.UInt;

    No_Error: Boolean := True;
  begin
    -- Load pixels
    Load_Pixels_From_File (This, Path, No_Error);

    if not No_Error then
      Success := False;
      return;
    end if;

    -- Go through pixels
    for K in This.Pixels.all'Range loop
      if This.Pixels.all (K) = Colour then
        -- Make transparent
        This.Pixels.all (K) := 16#00_FF_FF_FF#;
      end if;
    end loop;

    -- Create texture
    Load_Texture_From_Pixels_32 (This, No_Error);
    Success := No_Error;
  end Load_Texture_From_File_With_Colour_Key;

  ---------------------------------------------------------------------------

  procedure Load_Texture_From_File
    ( This: in out Texture; Path: in String; Success: out Boolean
    )
  is
    use type IL.Bool;
    use type GL.UInt;

    Texture_Loaded: Boolean := False;
    IMG_ID    : IL.UInt := 0;
    Suc       : IL.Bool := IL.IL_FALSE;
    Img_Width : GL.UInt;
    Img_Height: GL.UInt;
    Tex_Width : GL.UInt;
    Tex_Height: GL.UInt;
  begin
    -- Generate and set current image ID.
    IL.Gen_Images (1, IMG_ID'Address);
    IL.Bind_Image (IMG_ID);

    -- Load image.
    Suc := IL.Load_Image (Path);

    -- Image loaded successfuly.
    if Suc = IL.IL_TRUE then
      -- Convert image to RGBA
      Suc := IL.Convert_Image (IL.IL_RGBA, IL.IL_UNSIGNED_BYTE);
      if Suc = IL.IL_TRUE then
        -- Initialize dimensions
        Img_Width   := GL.UInt (IL.Get_Integer (IL.IL_IMAGE_WIDTH));
        Img_Height  := GL.UInt (IL.Get_Integer (IL.IL_IMAGE_HEIGHT));
        -- Calculate required texture dimensions
        Tex_Width   := Power_Of_Two (Img_Width);
        Tex_Height  := Power_Of_Two (Img_Height);

        -- Texture is wrong size
        if Img_Width /= Tex_Width or Img_Height /= Tex_Height then
          -- Place the image at upper left.
          ILU.Image_Parameter (ILU.ILU_PLACEMENT, ILU.ILU_UPPER_LEFT);
          -- Resize Image
          Suc := ILU.Enlarge_Canvas  (IL.UInt (Tex_Width), IL.UInt (Tex_Height), 1);
        end if;

        -- Create texture from file pixels.
        Load_From_Pixels_32
          ( This,
            GL.Pointer (IL.Get_Data),
            Img_Width,
            Img_Height,
            Tex_Width,
            Tex_Height,
            Texture_Loaded
          );
      end if;
      -- Delete file from memory
      IL.Delete_Images (1, IMG_ID'Address);
    end if;

    if not Texture_Loaded then
      TIO.Put_Line (TIO.Standard_Error, "Unable to load: " & Path);
    end if;
    Success := Texture_Loaded;
  end Load_Texture_From_File;

  ---------------------------------------------------------------------------

  procedure Load_From_Pixels_32
    ( This      : in  out Texture;
      Pixels    : in      GL.Pointer;
      Img_Width : in      GL.UInt;
      Img_Height: in      GL.UInt;
      Tex_Width : in      GL.UInt;
      Tex_Height: in      GL.UInt;
      Success   :     out Boolean
    )
  is
  begin
    -- Free texture if it exists
    Delete_Texture (This);

    -- Get texture dimensions
    This.Img_Width  := Img_Width;
    This.Img_Height := Img_Height;
    This.Width      := Tex_Width;
    This.Height     := Tex_Height;

    -- Generate texture ID
    GL.Gen_Textures (1, This.ID'Address);

    -- Bind texture ID
    GL.Bind_Texture (GL.GL_TEXTURE_2D, This.ID);

    -- Generate texture
    GL.Tex_Image
      ( GL.GL_TEXTURE_2D, 0, GL.GL_RGBA, GL.Int (This.Width),
        GL.Int (This.Height), 0, GL.GL_RGBA, GL.GL_UNSIGNED_BYTE, Pixels
      );

    -- Set texture parameters
    GL.Tex_Parameter (GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);
    GL.Tex_Parameter (GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR);

    -- Unbind texture
    GL.Bind_Texture (GL.GL_TEXTURE_2D, 0);

    -- Check for error
    declare
      use type GL.Enum;
      Error: constant GL.Enum := GL.Get_Error;
    begin
      if Error /= GL.GL_NO_ERROR then
        TIO.Put_Line (TIO.Standard_Error, "Error initializing OpenGL!");
        Success := False;
      end if;
    end;
    Success := True;
  end Load_From_Pixels_32;

  ---------------------------------------------------------------------------

  procedure Delete_Texture (This: in out Texture)
  is
    use type GL.UInt;
  begin
    if This.ID /= 0 then
      GL.Delete_Textures (1, This.ID'Address);
      This.ID := 0;
    end if;

    if This.Pixels /= Null then
      Free (This.Pixels);
      This.Pixels := Null;
    end if;

    This.Width      := 0;
    This.Height     := 0;
    This.Img_Width  := 0;
    This.Img_Height := 0;
  end Delete_Texture;

  ---------------------------------------------------------------------------

  procedure Render
    ( This: in  out Texture;
      X   : in      Float;
      Y   : in      Float;
      Do_Clip: in   Boolean;
      Clip: in      Rect.Rect_F := Rect.Rect_F'(0.0, 0.0, 0.0, 0.0)
    )
  is
    use type GL.UInt;
  begin
    -- If the texture exists
    if ID (This) /= 0 then
      -- Remove any previous transformations
      GL.Load_Identity;

      declare
        -- Texture Coordinates
        Tex_Top   : Float := 0.0;
        Tex_Bottom: Float := Float (This.Img_Height) / Float (This.Height);
        Tex_Left  : Float := 0.0;
        Tex_Right : Float := Float (This.Img_Width) / Float (This.Width);

        -- Vertex Coordinates
        Quad_Width  : Float := Float (This.Img_Width);
        Quad_Height : Float := Float (This.Img_Height);
      begin
        -- Handle clipping
        if Do_Clip then
          -- Tex coordinates
          Tex_Left    := Clip.X / Float (This.Width);
          Tex_Right   := (Clip.X + Clip.W) / Float (This.Width);
          Tex_Bottom  := Clip.Y / Float (This.Height);
          Tex_Top     := (Clip.Y + Clip.H) / Float (This.Height); 

          -- Vertex coordinates
          Quad_Width  := Clip.W;
          Quad_Height := Clip.H;
        end if;

        -- Move to rendering point
        GL.Translate (X, Y, 0.0);

        -- Set texture ID
        GL.Bind_Texture (GL.GL_TEXTURE_2D, This.ID);

        -- Render textured quad
        GL.Begin_Primitive (GL.GL_QUADS);
        begin
          GL.Tex_Coord  (Tex_Left, Tex_Top);
          GL.Vertex     (Float (0.0), 0.0);
          GL.Tex_Coord  (Tex_Right, Tex_Top);
          GL.Vertex     (Quad_Width, 0.0);
          GL.Tex_Coord  (Tex_Right, Tex_Bottom);
          GL.Vertex     (Quad_Width, Quad_Height);
          GL.Tex_Coord  (Tex_Left, Tex_Bottom);
          GL.Vertex     (Float (0.0), Quad_Height);
        end;
        GL.End_Primitive;
      end;
    end if;
  end Render;

  ---------------------------------------------------------------------------

  function ID (This: in Texture) return GL.UInt
  is
  begin
    return This.ID;
  end ID;

  ---------------------------------------------------------------------------

  function Width (This: in Texture) return GL.UInt
  is
  begin
    return This.Width;
  end Width;

  ---------------------------------------------------------------------------

  function Height (This: in Texture) return GL.UInt
  is
  begin
    return This.Height;
  end Height;

  ---------------------------------------------------------------------------

  function Img_Width (This: in Texture) return GL.UInt
  is
  begin
    return This.Img_Width;
  end Img_Width;

  ---------------------------------------------------------------------------

  function Img_Height (This: in Texture) return GL.UInt
  is
  begin
    return This.Img_Height;
  end Img_Height;

  ---------------------------------------------------------------------------

  function Power_Of_Two (Num: in GL.UInt) return GL.UInt
  is
    use type GL.UInt;

    N: GL.UInt := Num;
  begin
    if Num /= 0 then
      N := N - 1;
      N := N or (N / (2**1));   -- Or first 2 bits
      N := N or (N / (2**2));   -- Or next 2 bits
      N := N or (N / (2**4));   -- Or next 4 bits
      N := N or (N / (2**8));   -- Or next 8 bits
      N := N or (N / (2**16));  -- Or next 16 bits
      N := N + 1;
    end if;

    return N;
  end Power_Of_Two;

  ---------------------------------------------------------------------------

  procedure Lock (This: in out Texture; Success: out Boolean)
  is
    use type GL.UInt;
  begin
    if This.Pixels = Null and This.ID /= 0 then
      declare
        Size: constant GL.UInt := This.Width * This.Height;
      begin
        This.Pixels := new UInt_Array (0 .. Natural (Size) - 1);

        -- Set current texture
        GL.Bind_Texture (GL.GL_TEXTURE_2D, This.ID);

        -- Get pixels
        GL.Get_Tex_Image
          ( GL.GL_TEXTURE_2D,
            0,
            GL.GL_RGBA,
            GL.GL_UNSIGNED_BYTE,
            This.Pixels.all'Address
          );

        -- Unbind textures
        GL.Bind_Texture (GL.GL_TEXTURE_2D, 0);

        Success := True;
      end;
    else
      Success := False;
    end if;
  end Lock;

  ---------------------------------------------------------------------------

  procedure Unlock (This: in out Texture; Success: out Boolean)
  is
    use type GL.UInt;
  begin
    if This.Pixels /= Null and This.ID /= 0 then
      -- Set current texture
      GL.Bind_Texture (GL.GL_TEXTURE_2D, This.ID);

      -- Update texture
      GL.Tex_Sub_Image
        ( GL.GL_TEXTURE_2D, 0, 0, 0,
          Natural (This.Width),
          Natural (This.Height),
          GL.GL_RGBA,
          GL.GL_UNSIGNED_BYTE,
          This.Pixels.all'Address
        );

      -- Delete pixels
      Free (This.Pixels);
      This.Pixels := Null;

      -- Unbind texture
      GL.Bind_Texture (GL.GL_TEXTURE_2D, 0);

      Success := True;
    else
      Success := False;
    end if;
  end Unlock;

  ---------------------------------------------------------------------------

  function Get_Pixel_Data_32 (This: in Texture) return UInt_Array_Access
  is
  begin
    return This.Pixels;
  end Get_Pixel_Data_32;

  ---------------------------------------------------------------------------

  function Get_Pixel_32
    ( This: in Texture;
      X   : in GL.UInt;
      Y   : in GL.UInt
    ) return GL.UInt
  is
    use type GL.UInt;
  begin
    return This.Pixels (Natural (Y * This.Width + X));
  end Get_Pixel_32;

  ---------------------------------------------------------------------------

  procedure Set_Pixel_32
    ( This  : in out  Texture;
      X     : in      GL.UInt;
      Y     : in      GL.UInt;
      Pixel : in      GL.UInt
    )
  is
    use type GL.UInt;
  begin
    This.Pixels.all (Natural (Y * This.Width + X)) := Pixel;
  end Set_Pixel_32;

  ---------------------------------------------------------------------------

end Texture;
