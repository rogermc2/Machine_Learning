
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
   bkg_buf        : constant p_Byte_Array := null;
   forgive_errors : constant Boolean := False;
   error          : Boolean;
   img_buf        : p_Byte_Array := null;
   bkg            : GID.Image_descriptor;

   generic
      correct_orientation : GID.Orientation;
      --  Load image into a 24-bit truecolor BGR raw bitmap (for a BMP output)
   procedure Load_raw_image (image                 : in out GID.Image_descriptor;
                             buffer                : in out p_Byte_Array;
                             next_frame            : out Ada.Calendar.Day_Duration;
                             background_image_name : Unbounded_String);

   procedure Load_raw_image
     (image                 : in out GID.Image_descriptor;
      buffer                : in out p_Byte_Array;
      next_frame            : out Ada.Calendar.Day_Duration;
      background_image_name : Unbounded_String) is
      subtype Primary_color_range is Unsigned_8;
      subtype U16 is Unsigned_16;
      image_width           : constant Positive := GID.Pixel_width (image);
      image_height          : constant Positive := GID.Pixel_height (image);
      padded_line_size_x    : constant Positive :=
                                4 * Integer (Float'Ceiling (Float (image_width) * 3.0 / 4.0));
      padded_line_size_y    : constant Positive :=
                                4 * Integer (Float'Ceiling (Float (image_height) * 3.0 / 4.0));
      --  (in bytes)
      idx                   : Integer;
      mem_x                 : Natural;
      mem_y                 : Natural;
      bkg_padded_line_size  : Positive;
      bkg_width             : Natural;
      bkg_height            : Natural;

      procedure Set_X_Y (x, y : Natural) is
         pragma Inline (Set_X_Y);
         use GID;
         rev_x : constant Natural := image_width - (x + 1);
         rev_y : constant Natural := image_height - (y + 1);
      begin
         case correct_orientation is
         when Unchanged =>
            idx := 3 * x + padded_line_size_x * y;
         when Rotation_90 =>
            idx := 3 * rev_y + padded_line_size_y * x;
         when Rotation_180 =>
            idx := 3 * rev_x + padded_line_size_x * rev_y;
         when Rotation_270 =>
            idx := 3 * y + padded_line_size_y * rev_x;
         end case;

         mem_x := x;
         mem_y := y;
      end Set_X_Y;

      --  No background version of Put_Pixel
      procedure Put_Pixel_without_bkg
        (red, green, blue : Primary_color_range; alpha : Primary_color_range) is
         pragma Inline (Put_Pixel_without_bkg);
         pragma Warnings (off, alpha); -- alpha is ignored
         use GID;
      begin
         buffer (idx .. idx + 2) := (blue, green, red);
         --  GID requires us to look at the next pixel for next time:
         case correct_orientation is
         when Unchanged =>
            idx := idx + 3;
         when Rotation_90 =>
            idx := idx + padded_line_size_y;
         when Rotation_180 =>
            idx := idx - 3;
         when Rotation_270 =>
            idx := idx - padded_line_size_y;
         end case;
      end Put_Pixel_without_bkg;

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
      procedure Put_Pixel_with_image_bkg
        (red, green, blue : Primary_color_range;
         alpha            : Primary_color_range) is
         pragma Inline (Put_Pixel_with_image_bkg);
         b_red,
         b_green,
         b_blue  : Primary_color_range;
         bkg_idx : Natural;
      begin
         if alpha = 255 then
            buffer (idx .. idx + 2) := (blue, green, red);
         else -- blend with background image
            bkg_idx := 3 * (mem_x mod bkg_width) + bkg_padded_line_size * (mem_y mod bkg_height);
            b_blue := bkg_buf (bkg_idx);
            b_green := bkg_buf (bkg_idx + 1);
            b_red  := bkg_buf (bkg_idx + 2);
            buffer (idx)  := Primary_color_range ((U16 (alpha) * U16 (blue)  + U16 (255 - alpha) * U16 (b_blue)) / 255);
            buffer (idx + 1) := Primary_color_range ((U16 (alpha) * U16 (green) + U16 (255 - alpha) * U16 (b_green)) / 255);
            buffer (idx + 2) := Primary_color_range ((U16 (alpha) * U16 (red)   + U16 (255 - alpha) * U16 (b_red)) / 255);
         end if;

         idx := idx + 3;
         --  ^ GID requires us to look to next pixel on the right for next time.
         mem_x := mem_x + 1;
      end Put_Pixel_with_image_bkg;

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

      procedure BMP24_Load_without_bkg is
        new GID.Load_image_contents (Primary_color_range, Set_X_Y,
                                     Put_Pixel_without_bkg, Feedback, GID.fast);

      procedure BMP24_Load_with_unicolor_bkg is
        new GID.Load_image_contents (Primary_color_range, Set_X_Y,
                                     Put_Pixel_with_unicolor_bkg, Feedback,
                                     GID.fast);

      procedure BMP24_Load_with_image_bkg is
        new GID.Load_image_contents (Primary_color_range, Set_X_Y,
                                     Put_Pixel_with_image_bkg, Feedback,
                                     GID.fast);

      --  -------------------------------------------------------------------------

   begin
      error := False;
      Dispose (buffer);

      case correct_orientation is
      when GID.Unchanged | GID.Rotation_180 =>
         buffer := new Byte_Array
           (0 .. padded_line_size_x * GID.Pixel_height (image) - 1);
      when GID.Rotation_90 | GID.Rotation_270 =>
         buffer := new
           Byte_Array (0 .. padded_line_size_y * GID.Pixel_width (image) - 1);
      end case;

      if GID.Expect_transparency (image) then
         if background_image_name = Null_Unbounded_String then
            BMP24_Load_with_unicolor_bkg (image, next_frame);
         else
            bkg_width := GID.Pixel_width (bkg);
            bkg_height := GID.Pixel_height (bkg);
            bkg_padded_line_size :=
              4 * Integer (Float'Ceiling (Float (bkg_width) * 3.0 / 4.0));
            BMP24_Load_with_image_bkg (image, next_frame);
         end if;
      else
         BMP24_Load_without_bkg (image, next_frame);
      end if;

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

   procedure Load_raw_image_0 is new Load_raw_image (GID.Unchanged);
   procedure Load_raw_image_90 is new Load_raw_image (GID.Rotation_90);
   procedure Load_raw_image_180 is new Load_raw_image (GID.Rotation_180);
   procedure Load_raw_image_270 is new Load_raw_image (GID.Rotation_270);

   --  -------------------------------------------------------------------------

   procedure Dump_BMP_24 (name : String; Image_desc : GID.Image_descriptor) is
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
      Put_Line ("Dump_BMP_24 img_buf.all'Length " &
                  Integer'Image (img_buf.all'Length));

      FileHeader.bfSize := FileHeader.bfOffBits + FileInfo.biSizeImage;

      Put_Line ("Dump_BMP_24 creating " & name & ".dib");
      Create (out_file_id, Out_File, name & ".dib");
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

   procedure Process (name : String; image_name : in out Unbounded_String) is
      Routine_Name  : constant String := "Simple_Support.Process ";
      up_name       : constant String := To_Upper (name);
      in_file_id    : Ada.Streams.Stream_IO.File_Type;
      image_desc    : GID.Image_descriptor;
      next_frame    : Ada.Calendar.Day_Duration := 0.0;
--        current_frame : Ada.Calendar.Day_Duration := 0.0;
      Done          : Boolean := False;
   begin
      --  Load the image in its original format
      Open (in_file_id, In_File, name);
      Put_Line (Routine_Name & "processing " & name);

      GID.Load_image_header
        (image_desc, Stream (in_file_id).all,
         try_tga =>
           name'Length >= 4 and then
         up_name (up_name'Last - 3 .. up_name'Last) = ".TGA");

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

      --        if as_background then
      --           Put_Line ("as_background");
      --           case GID.Display_orientation (image_desc) is
      --           when GID.Unchanged =>
      --              Load_raw_image_0 (image_desc, bkg_buf, next_frame,
      --                                background_image_name);
      --           when GID.Rotation_90 =>
      --              Load_raw_image_90 (image_desc, bkg_buf, next_frame,
      --                                 background_image_name);
      --           when GID.Rotation_180 =>
      --              Load_raw_image_180 (image_desc, bkg_buf, next_frame,
      --                                  background_image_name);
      --           when GID.Rotation_270 =>
      --              Load_raw_image_270 (image_desc, bkg_buf, next_frame,
      --                                  background_image_name);
      --           end case;
      --
      --           bkg := image_desc;
      --           New_Line;
      --
      --        else  --  not as_background"
      while not Done loop
         case GID.Display_orientation (image_desc) is
            when GID.Unchanged =>
               Load_raw_image_0 (image_desc, img_buf, next_frame,
                                 image_name);
            when GID.Rotation_90 =>
               Load_raw_image_90 (image_desc, img_buf, next_frame,
                                  image_name);
            when GID.Rotation_180 =>
               Load_raw_image_180 (image_desc, img_buf, next_frame,
                                   image_name);
            when GID.Rotation_270 =>
               Load_raw_image_270 (image_desc, img_buf, next_frame,
                                   image_name);
         end case;

         Dump_BMP_24 (name,image_desc);
         New_Line;

         if error then
            Put_Line (Routine_Name & "Error!");
         end if;

         Done := next_frame = 0.0;
--           current_frame := next_frame;
      end loop;
      --     end if;
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
