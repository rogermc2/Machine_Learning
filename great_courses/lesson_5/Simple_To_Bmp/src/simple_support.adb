
with GID;

with Ada.Calendar;
with Ada.Characters.Handling;     use Ada.Characters.Handling;
with Ada.Streams.Stream_IO;       use Ada.Streams.Stream_IO;
with Ada.Text_IO;                 use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Interfaces; use Interfaces;

package body Simple_Support is

   type Byte_Array is array (Integer range <>) of Unsigned_8;
   type p_Byte_Array is access Byte_Array;

   procedure Dispose is new Ada.Unchecked_Deallocation
     (Byte_Array, p_Byte_Array);

   stars          : Natural := 0;
--     bkg_buf        : constant p_Byte_Array := null;
   forgive_errors : constant Boolean := False;
   error          : Boolean;
   img_buf        : p_Byte_Array := null;
--     bkg            : GID.Image_descriptor;

   procedure Load_raw_image
     (image                 : in out GID.Image_descriptor;
      buffer                : out p_Byte_Array;
      next_frame            : out Ada.Calendar.Day_Duration) is
--        background_image_name : Unbounded_String) is
      subtype Primary_color_range is Unsigned_8;
      subtype U16 is Unsigned_16;
      image_width           : constant Positive := GID.Pixel_width (image);
      padded_line_size_x    : constant Positive :=
                                4 * Integer (Float'Ceiling (Float (image_width) * 3.0 / 4.0));
      --  (in bytes)
      idx                   : Integer;
--        mem_x                 : Natural;
--        mem_y                 : Natural;
--        bkg_padded_line_size  : Positive;
--        bkg_width             : Natural;
--        bkg_height            : Natural;

      procedure Set_X_Y (x, y : Natural) is
         pragma Inline (Set_X_Y);
      begin
         idx := 3 * x + padded_line_size_x * y;
--           mem_x := x;
--           mem_y := y;
      end Set_X_Y;

      --  No background version of Put_Pixel
--        procedure Put_Pixel_without_bkg
--          (red, green, blue : Primary_color_range; alpha : Primary_color_range) is
--           pragma Inline (Put_Pixel_without_bkg);
--           pragma Warnings (off, alpha); -- alpha is ignored
--        begin
--           buffer (idx .. idx + 2) := (blue, green, red);
--           --  GID requires us to look at the next pixel for next time:
--           idx := idx + 3;
--        end Put_Pixel_without_bkg;

      --  -------------------------------------------------------------------------
      --  Unicolor background version of Put_Pixel
      procedure Put_Pixel_with_unicolor_bkg
        (red, green, blue : Primary_color_range; alpha : Primary_color_range) is
         pragma Inline (Put_Pixel_with_unicolor_bkg);
         u_red   : constant := 200;
         u_green : constant := 133;
         u_blue  : constant := 32;
      begin
         if alpha = 255 then
            buffer (idx .. idx + 2) := (blue, green, red);
         else -- blend with bckground color
            buffer (idx)  := Primary_color_range ((U16 (alpha) * U16 (blue)  + U16 (255 - alpha) * u_blue) / 255);
            buffer (idx + 1) := Primary_color_range ((U16 (alpha) * U16 (green) + U16 (255 - alpha) * u_green) / 255);
            buffer (idx + 2) := Primary_color_range ((U16 (alpha) * U16 (red)   + U16 (255 - alpha) * u_red) / 255);
         end if;
         idx := idx + 3;
         --  ^ GID requires us to look to next pixel on the right for next time.
      end Put_Pixel_with_unicolor_bkg;

      --  -------------------------------------------------------------------------
      --  Background image version of Put_Pixel
--        procedure Put_Pixel_with_image_bkg
--          (red, green, blue : Primary_color_range;
--           alpha            : Primary_color_range) is
--           pragma Inline (Put_Pixel_with_image_bkg);
--           b_red,
--           b_green,
--           b_blue  : Primary_color_range;
--           bkg_idx : Natural;
--        begin
--           if alpha = 255 then
--              buffer (idx .. idx + 2) := (blue, green, red);
--           else -- blend with background image
--              bkg_idx := 3 * (mem_x mod bkg_width) + bkg_padded_line_size * (mem_y mod bkg_height);
--              b_blue := bkg_buf (bkg_idx);
--              b_green := bkg_buf (bkg_idx + 1);
--              b_red  := bkg_buf (bkg_idx + 2);
--              buffer (idx)  := Primary_color_range ((U16 (alpha) * U16 (blue)  + U16 (255 - alpha) * U16 (b_blue)) / 255);
--              buffer (idx + 1) := Primary_color_range ((U16 (alpha) * U16 (green) + U16 (255 - alpha) * U16 (b_green)) / 255);
--              buffer (idx + 2) := Primary_color_range ((U16 (alpha) * U16 (red)   + U16 (255 - alpha) * U16 (b_red)) / 255);
--           end if;
--
--           idx := idx + 3;
--           --  ^ GID requires us to look to next pixel on the right for next time.
--           mem_x := mem_x + 1;
--        end Put_Pixel_with_image_bkg;

      --  -------------------------------------------------------------------------

      procedure Feedback (percents : Natural) is
         so_far : constant Natural := percents / 5;
      begin
         for i in stars + 1 .. so_far loop
            Put ('*');
         end loop;
         stars := so_far;
      end Feedback;

      --  -------------------------------------------------------------------------

      --  Here, the exciting thing: the instanciation of
      --  GID.Load_image_contents. In our case, we load the image
      --  into a 24-bit bitmap (because we provide a Put_Pixel
      --  that does that with the pixels), but we could do plenty
      --  of other things instead, like display the image live on a GUI.

      --  More exciting: for tuning performance, we have 3 different
      --  instances of GID.Load_image_contents (each of them with the full
      --  decoders for all formats, own specialized generic instances, inlines,
      --  etc.) depending on the transparency features.

--        procedure BMP24_Load_without_bkg is
--          new GID.Load_image_contents (Primary_color_range, Set_X_Y,
--                                       Put_Pixel_without_bkg, Feedback, GID.fast);

      procedure BMP24_Load_with_unicolor_bkg is
        new GID.Load_image_contents (Primary_color_range, Set_X_Y,
                                     Put_Pixel_with_unicolor_bkg, Feedback,
                                     GID.fast);

--        procedure BMP24_Load_with_image_bkg is
--          new GID.Load_image_contents (Primary_color_range, Set_X_Y,
--                                       Put_Pixel_with_image_bkg, Feedback,
--                                       GID.fast);

      --  -------------------------------------------------------------------------

   begin
      error := False;
      Dispose (buffer);

      buffer := new Byte_Array
        (0 .. padded_line_size_x * GID.Pixel_height (image) - 1);

--        if GID.Expect_transparency (image) then
--           Put_Line ("Expect_transparency");
--           if background_image_name = Null_Unbounded_String then
--              Put_Line ("background_image_name is null");
            BMP24_Load_with_unicolor_bkg (image, next_frame);
--           else
--           Put_Line ("background_image_name not null");
--              bkg_width := GID.Pixel_width (bkg);
--              bkg_height := GID.Pixel_height (bkg);
--              bkg_padded_line_size :=
--                4 * Integer (Float'Ceiling (Float (bkg_width) * 3.0 / 4.0));
--              BMP24_Load_with_image_bkg (image, next_frame);
--           end if;
--        else
--           Put_Line ("don't Expect_transparency");
--           BMP24_Load_without_bkg (image, next_frame);
--        end if;

   exception
      when others =>
         if forgive_errors then
            error := True;
            next_frame := 0.0;
         else
            raise;
         end if;

   end Load_raw_image;

   --  -------------------------------------------------------------------------

   procedure Dump_BMP_24 (File_Name  : String;
                          Image_desc : GID.Image_descriptor) is
      type BITMAPFILEHEADER is record
         bfType      : Unsigned_16;
         bfSize      : Unsigned_32;
         bfReserved1 : Unsigned_16 := 0;
         bfReserved2 : Unsigned_16 := 0;
         bfOffBits   : Unsigned_32;
      end record;

      --  ^ No packing needed
      BITMAPFILEHEADER_Bytes : constant := 14;

      type BITMAPINFOHEADER is record
         biSize          : Unsigned_32;
         biWidth         : Unsigned_32;
         biHeight        : Unsigned_32;
         biPlanes        : Unsigned_16 := 1;
         biBitCount      : Unsigned_16;
         biCompression   : Unsigned_32 := 0;
         biSizeImage     : Unsigned_32;
         biXPelsPerMeter : Unsigned_32 := 0;
         biYPelsPerMeter : Unsigned_32 := 0;
         biClrUsed       : Unsigned_32 := 0;
         biClrImportant  : Unsigned_32 := 0;
      end record;
      --  ^ No packing needed
      BITMAPINFOHEADER_Bytes : constant := 40;

      FileInfo    : BITMAPINFOHEADER;
      FileHeader  : BITMAPFILEHEADER;
      out_file_id : Ada.Streams.Stream_IO.File_Type;

      --  ----------------------------------------------------------------------

      generic
         type Number is mod <>;
      procedure Write_Intel_x86_number (n : in Number);

      procedure Write_Intel_x86_number (n : in Number) is
         m     : Number := n;
         bytes : constant Integer := Number'Size / 8;
      begin
         for i in 1 .. bytes loop
            Unsigned_8'Write (Stream (out_file_id), Unsigned_8 (m and 255));
            m := m / 256;
         end loop;
      end Write_Intel_x86_number;

      --  ----------------------------------------------------------------------

      procedure Write_Intel is new Write_Intel_x86_number (Unsigned_16);
      procedure Write_Intel is new Write_Intel_x86_number (Unsigned_32);
   begin
      FileHeader.bfType := 16#4D42#; -- 'BM'
      FileHeader.bfOffBits := BITMAPINFOHEADER_Bytes + BITMAPFILEHEADER_Bytes;
      FileInfo.biSize       := BITMAPINFOHEADER_Bytes;
      case GID.Display_orientation (Image_desc) is
      when GID.Unchanged | GID.Rotation_180 =>
         FileInfo.biWidth  := Unsigned_32 (GID.Pixel_width (Image_desc));
         FileInfo.biHeight := Unsigned_32 (GID.Pixel_height (Image_desc));
      when GID.Rotation_90 | GID.Rotation_270 =>
         FileInfo.biWidth  := Unsigned_32 (GID.Pixel_height (Image_desc));
         FileInfo.biHeight := Unsigned_32 (GID.Pixel_width (Image_desc));
      end case;

      FileInfo.biBitCount   := 24;
      FileInfo.biSizeImage  := Unsigned_32 (img_buf.all'Length);
      New_Line;
      FileHeader.bfSize := FileHeader.bfOffBits + FileInfo.biSizeImage;

      Put_Line ("Dump_BMP_24 creating " & File_Name & ".dib");
      Create (out_file_id, Out_File, File_Name & ".dib");
      --  BMP Header, endian-safe:
      Write_Intel (FileHeader.bfType);
      Write_Intel (FileHeader.bfSize);
      Write_Intel (FileHeader.bfReserved1);
      Write_Intel (FileHeader.bfReserved2);
      Write_Intel (FileHeader.bfOffBits);

      Write_Intel (FileInfo.biSize);
      Write_Intel (FileInfo.biWidth);
      Write_Intel (FileInfo.biHeight);
      Write_Intel (FileInfo.biPlanes);
      Write_Intel (FileInfo.biBitCount);
      Write_Intel (FileInfo.biCompression);
      Write_Intel (FileInfo.biSizeImage);
      Write_Intel (FileInfo.biXPelsPerMeter);
      Write_Intel (FileInfo.biYPelsPerMeter);
      Write_Intel (FileInfo.biClrUsed);
      Write_Intel (FileInfo.biClrImportant);
      --  BMP raw BGR image:
      declare
         --  Workaround for the severe xxx'Read xxx'Write performance
         --  problems in the GNAT and ObjectAda compilers (as in 2009)
         --  This is possible if and only if Byte = Stream_Element and
         --  arrays types are both packed the same way.
         --
         subtype Size_test_a is Byte_Array (1 .. 19);
         subtype Size_test_b is Ada.Streams.Stream_Element_Array (1 .. 19);
         workaround_possible : constant Boolean :=
                                 Size_test_a'Size = Size_test_b'Size and then
                                     Size_test_a'Alignment =
                                       Size_test_b'Alignment;
      begin
         if workaround_possible then
            declare
               use Ada.Streams;
               SE_Buffer   : Stream_Element_Array
                 (0 .. Stream_Element_Offset (img_buf'Length - 1));
               for SE_Buffer'Address use img_buf.all'Address;
               pragma Import (Ada, SE_Buffer);
            begin
               Ada.Streams.Write
                 (Stream (out_file_id).all,
                  SE_Buffer (0 .. Stream_Element_Offset (img_buf'Length - 1)));
            end;
         else
            --  the workaround is about this line...
            Byte_Array'Write (Stream (out_file_id), img_buf.all);
         end if;
      end;
      Close (out_file_id);

   end Dump_BMP_24;

   --  -------------------------------------------------------------------------

   procedure Process (Image_File_Name : String) is
      Routine_Name    : constant String := "Simple_Support.Process ";
      File_Name_Upper : constant String := To_Upper (Image_File_Name);
      in_file_id      : Ada.Streams.Stream_IO.File_Type;
      image_desc      : GID.Image_descriptor;
      next_frame      : Ada.Calendar.Day_Duration := 0.0;
      Done            : Boolean := False;
   begin
      --  Load the image in its original format
      Open (in_file_id, In_File, Image_File_Name);
      Put_Line (Routine_Name & "processing " & Image_File_Name);

      GID.Load_image_header
        (image_desc, Stream (in_file_id).all,
         try_tga =>
           Image_File_Name'Length >= 4 and then
         File_Name_Upper
           (File_Name_Upper'Last - 3 .. File_Name_Upper'Last) = ".TGA");

      Put_Line ("Image format: " &
                  GID.Image_format_type'Image (GID.Format (image_desc)));
      Put_Line ("Image detailed format: " &
                  GID.Detailed_format (image_desc));
      Put_Line ("Image sub-format ID (if any): " &
                  Integer'Image (GID.Subformat (image_desc)));
      Put_Line ("Dimensions in pixels: " &
                  Integer'Image (GID.Pixel_width (image_desc)) & " x" &
                  Integer'Image (GID.Pixel_height (image_desc)));
      Put_Line (Standard_Error,"  Display orientation: " &
                  GID.Orientation'Image (GID.Display_orientation (image_desc)));
      Put ("Color depth: " &
             Integer'Image (GID.Bits_per_pixel (image_desc)) & " bits");

      if GID.Bits_per_pixel (image_desc) <= 24 then
         Put_Line (',' & Integer'Image (2**GID.Bits_per_pixel (image_desc)) &
                     " colors");
      else
         New_Line;
      end if;

      Put_Line ("Palette: " &
                  Boolean'Image (GID.Has_palette (image_desc)));
      Put_Line ("Greyscale: " & Boolean'Image (GID.Greyscale (image_desc)));
      Put_Line ("RLE encoding (if any): " &
                  Boolean'Image (GID.Is_RLE_encoded (image_desc)));
      Put_Line ("Interlaced (GIF: each frame's choice): " &
                  Boolean'Image (GID.Is_Interlaced (image_desc)));
      Put_Line ("Expect transparency: " &
                  Boolean'Image (GID.Expect_transparency (image_desc)));
      Put_Line ("1........10........20");
      Put_Line ("         |         | ");

      while not Done loop
--           Load_raw_image (image_desc, img_buf, next_frame, image_name);
         Load_raw_image (image_desc, img_buf, next_frame);
         Dump_BMP_24 (Image_File_Name, image_desc);
         New_Line;

         if error then
            Put_Line (Routine_Name & "Error!");
         end if;

         Done := next_frame = 0.0;
      end loop;
      Close (in_file_id);

   exception
      when GID.unknown_image_format =>
         Put_Line (Routine_Name & "image format is unknown!");
         if Is_Open (in_file_id) then
            Close (in_file_id);
         end if;

   end Process;

   --  -------------------------------------------------------------------------

end Simple_Support;
