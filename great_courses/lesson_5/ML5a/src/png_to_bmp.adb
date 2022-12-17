
--  Derived from gid/test/To_BMP

with Ada.Assertions; use Ada.Assertions;

with GID;

with Ada.Calendar;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with ML_Arrays_And_Matrices;

package body PNG_To_BMP is
   use Interfaces;

   type p_Byte_Array is access ML_Arrays_And_Matrices.Byte_Array;

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

   stars                  : Natural := 0;
   img_buf                : p_Byte_Array := null;

   procedure Dispose is new Ada.Unchecked_Deallocation
     (ML_Arrays_And_Matrices.Byte_Array, p_Byte_Array);

   --  ---------------------------------------------------------------------------------------

   function Height (Data : Image_Array) return Natural is
   begin
      return Data'Length (2);
   end Height;

   --  -------------------------------------------------------------------------

   function Width (Data : Image_Array) return Natural is
   begin
      return Data'Length;
   end Width;

   --  -------------------------------------------------------------------------

   procedure Load_raw_image (image      : in out GID.Image_descriptor;
                             buffer     : out p_Byte_Array;
                             next_frame : out Ada.Calendar.Day_Duration) is
      subtype Primary_color_range is Unsigned_8;
      subtype U16 is Unsigned_16;
      image_width        : constant Positive := GID.Pixel_width (image);
      padded_line_size_x : constant Positive :=
                             4 * Integer (Float'Ceiling (Float (image_width) *
                                            3.0 / 4.0));
      idx                : Natural;  --  (in bytes)

      --  ----------------------------------------------------------------------

      procedure Set_X_Y (x, y : Natural) is
         pragma Inline (Set_X_Y);
      begin
         idx := 3 * x + padded_line_size_x * y;
      end Set_X_Y;

      --  ----------------------------------------------------------------------
      --  Unicolor background version of Put_Pixel
      procedure Put_Pixel
        (red, green, blue : Primary_color_range;
         alpha            : Primary_color_range) is
         pragma Inline (Put_Pixel);
         u_red   : constant := 200;
         u_green : constant := 133;
         u_blue  : constant := 32;
      begin
         if alpha = 255 then
            buffer (idx .. idx + 2) := (blue, green, red);
         else -- blend with bckground color
            buffer (idx) :=
              Primary_color_range ((U16 (alpha) * U16 (blue) +
                                     U16 (255 - alpha) * u_blue) / 255);
            buffer (idx + 1) :=
              Primary_color_range ((U16 (alpha) * U16 (green) +
                                     U16 (255 - alpha) * u_green) / 255);
            buffer (idx + 2) :=
              Primary_color_range ((U16 (alpha) * U16 (red) +
                                     U16 (255 - alpha) * u_red) / 255);
         end if;
         idx := idx + 3;
         --  ^ GID requires us to look to next pixel on the right for next time.
      end Put_Pixel;

      --  ----------------------------------------------------------------------

      procedure Feedback (percents : Natural) is
         so_far : constant Natural := percents / 5;
      begin
         for i in stars + 1 .. so_far loop
            Put ('*');
         end loop;
         stars := so_far;
      end Feedback;

      --  ----------------------------------------------------------------------
      --  The exciting thing: the instanciation of GID.Load_image_contents.
      --  Load the image into a 24-bit bitmap because we provide a
      --  Put_Pixel that does that with the pixels. We could do plenty of other
      --  things instead like display the image live on a GUI.

      procedure BMP24_Load is new GID.Load_image_contents
        (Primary_color_range, Set_X_Y, Put_Pixel, Feedback, GID.fast);

      --  ----------------------------------------------------------------------

   begin  --  Load_raw_image
      Dispose (buffer);

      buffer := new ML_Arrays_And_Matrices.Byte_Array
        (0 .. padded_line_size_x * GID.Pixel_height (image) - 1);
      BMP24_Load (image, next_frame);

   end Load_raw_image;

   --  -------------------------------------------------------------------------

   procedure Write_BMP_24 (File_Name  : String;
                           Image_desc : GID.Image_descriptor) is
      use Ada.Strings.Unbounded;
      use ML_Arrays_And_Matrices;
      Routine_Name : constant String := "Simple_BMP.Write_BMP_24 ";
      out_file_id  : Ada.Streams.Stream_IO.File_Type;

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

      procedure Write_Intel is new Write_Intel_x86_number (Unsigned_16);
      procedure Write_Intel is new Write_Intel_x86_number (Unsigned_32);

      --  ----------------------------------------------------------------------

      Out_File_Name : constant Unbounded_String :=
                        To_Unbounded_String (File_Name);
      Pos           : constant Natural := Index (Out_File_Name, ".png") - 1;
      FileInfo      : BITMAPINFOHEADER;
      FileHeader    : BITMAPFILEHEADER;
   begin  --  Write_BMP_24
      New_Line;
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

      Put_Line (Routine_Name & "creating " & Slice (Out_File_Name, 1, Pos) &
                  ".dib");
      --  ".dib": unknown synonym of ".bmp";
      Create (out_file_id, Out_File, Slice (Out_File_Name, 1, Pos) & ".dib");
      --  BMP Header, endian-safe:
      Write_Intel (FileHeader.bfType);        --  unsigned_32
      Write_Intel (FileHeader.bfSize);        --  unsigned_16
      Write_Intel (FileHeader.bfReserved1);   --  unsigned_32
      Write_Intel (FileHeader.bfReserved2);   --  unsigned_32
      Write_Intel (FileHeader.bfOffBits);    --  unsigned_16

      Write_Intel (FileInfo.biSize);    --  unsigned_16
      Write_Intel (FileInfo.biWidth);    --  unsigned_16
      Write_Intel (FileInfo.biHeight);    --  unsigned_16
      Write_Intel (FileInfo.biPlanes);     --  unsigned_32
      Write_Intel (FileInfo.biBitCount);   --  unsigned_32
      --  ther rest are unsigned_16
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
         subtype Size_test_a is ML_Arrays_And_Matrices.Byte_Array (1 .. 19);
         subtype Size_test_b is Ada.Streams.Stream_Element_Array (1 .. 19);
         workaround_possible : constant Boolean :=
                                 Size_test_a'Size = Size_test_b'Size and then
                                     Size_test_a'Alignment =
                                       Size_test_b'Alignment;
      begin
         if workaround_possible then
            declare
               use Ada.Streams;
               SE_Buffer : Stream_Element_Array
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
      end;  --  declare block
      Put_Line (Routine_Name & "img_buf.all length" &
                  Integer'Image (img_buf.all'Length));
      Close (out_file_id);

   end Write_BMP_24;

   --  -------------------------------------------------------------------------

   function Process (Image_File_Name : String) return Image_Array is
      Routine_Name    : constant String := "Simple_BMP.Process ";
      File_Name_Upper : constant String := To_Upper (Image_File_Name);
      in_file_id      : Ada.Streams.Stream_IO.File_Type;
      image_desc      : GID.Image_descriptor;
      Width           : Positive;
      Height          : Positive;
      next_frame      : Ada.Calendar.Day_Duration := 0.0;
      Buffer_Index    : Natural := 0;
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
      Width := GID.Pixel_width (image_desc);
      Height := GID.Pixel_height (image_desc);
--        Put_Line ("Image format: " &
--                    GID.Image_format_type'Image (GID.Format (image_desc)));
--        Put_Line ("Dimensions in pixels: " &
--                    Integer'Image (Width) & " x" & Integer'Image (Height));

      Load_raw_image (image_desc, img_buf, next_frame);
      Assert (next_frame = 0.0, "");
      Write_BMP_24 (Image_File_Name, image_desc);

      declare
         Image_Data : Image_Array (1 .. Height - 1, 1 .. Width + 1, 1 .. 3);
      begin
         for row in reverse Image_Data'Range loop
            for col in Image_Data'Range (2) loop
               for pix in Image_Data'Range (3) loop
                  Buffer_Index := Buffer_Index + 1;
                  if pix = 1 then
                     Image_Data (row, col, pix) := img_buf (Buffer_Index + 1);
                  elsif pix = 2 then
                     Image_Data (row, col, pix) := img_buf (Buffer_Index - 1);
                  else
                     Image_Data (row, col, pix) := img_buf (Buffer_Index);
                  end if;
               end loop;
            end loop;
         end loop;
         Close (in_file_id);
         return Image_Data;
      end;  --  declare block

   exception
      when GID.unknown_image_format =>
         Put_Line (Routine_Name & "image format is unknown!");
         if Is_Open (in_file_id) then
            Close (in_file_id);
         end if;
         raise;

   end Process;

   --  -------------------------------------------------------------------------

end PNG_To_BMP;
