
--  Derived from gid/test/To_BMP

with Ada.Assertions; use Ada.Assertions;
--  with Ada.Numerics;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GID;

with Ada.Calendar;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

--  with Maths;

with ML_Arrays_And_Matrices;

package body JPEG_To_BMP is
   use Interfaces;

   type p_Byte_Array is access ML_Arrays_And_Matrices.Byte_Array;

   img_buf : p_Byte_Array := null;

   procedure Dispose is new Ada.Unchecked_Deallocation
     (ML_Arrays_And_Matrices.Byte_Array, p_Byte_Array);

   --  ---------------------------------------------------------------------------------------

   procedure Load_raw_image (image      : in out GID.Image_descriptor;
                             buffer     : out p_Byte_Array;
                             next_frame : out Ada.Calendar.Day_Duration) is
      --                               Image_Format : Unbounded_String) is
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

      --        procedure Set_X_Y_R45 (x, y : Natural) is
      --           pragma Inline (Set_X_Y_R45);
      --           use Ada.Numerics;
      --           use Maths.Float_Math_Functions;
      --           use ML_Arrays_And_Matrices;
      --           use Real_Float_Arrays;
      --           Width     : constant Float := 1280.0;
      --           Height    : constant Float := 854.0;
      --           Point_1   : constant Real_Float_Vector (1 .. 2) := (Width / 2.0, Height / 2.0);
      --           Point_2   : constant Real_Float_Vector (1 .. 2) := (Float (x), Float (y));
      --           R45       : constant Float := Cos (0.25 * Pi);
      --           R45_Mat   : constant Real_Float_Matrix (1 .. 2, 1 .. 2) :=
      --             ((R45, -R45), (R45, R45));
      --           XY        : constant Real_Float_Vector (1 .. 2) :=
      --             R45_Mat * (Point_2 - Point_1) + Point_1;
      --        begin
      --           Put_Line ("Set_XY: " & Integer'Image (x) & Integer'Image (y));
      --           Put_Line ("Set_XY: " & Float'Image (XY (1)) & Float'Image (XY (2)));
      --           Put_Line ("Set_XY rotated: " & Float'Image (XY (1)) &
      --                       Float'Image (XY (2)));
      --           idx := 3 * Natural (XY (1)) + padded_line_size_x * Natural (XY (2));
      --
      --        end Set_X_Y_R45;

      --  ----------------------------------------------------------------------

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

      procedure Feedback (percents : Natural := 0) is
      begin
         null;
      end Feedback;

      --  ----------------------------------------------------------------------

      procedure BMP24_Load is new GID.Load_image_contents
        (Primary_color_range, Set_X_Y, Put_Pixel, Feedback, GID.fast);

      --  ----------------------------------------------------------------------

      --        procedure BMP24_Load_R45 is new GID.Load_image_contents
      --          (Primary_color_range, Set_X_Y_R45, Put_Pixel, Feedback, GID.fast);

      --  ----------------------------------------------------------------------

   begin  --  Load_raw_image
      Dispose (buffer);
      --  GID.Unchanged | GID.Rotation_180 =>
      buffer := new ML_Arrays_And_Matrices.Byte_Array
        (0 .. padded_line_size_x * GID.Pixel_height (image) - 1);
      --        if Image_Format = "PNG" then
      BMP24_Load (image, next_frame);
      --        elsif Image_Format = "JPEG" then
      --           BMP24_Load_R45 (image, next_frame);
      --        else
      --           raise Unsupported_Image_Format;
      --        end if;

   end Load_raw_image;

   --  -------------------------------------------------------------------------

   function Process (Image_File_Name : String) return Image_Array is
      Routine_Name    : constant String := "PNG_To_BMP.Process ";
      File_Name_Upper : constant String := To_Upper (Image_File_Name);
      File_Kind       : constant String :=
        File_Name_Upper (File_Name_Upper'Last - 3 .. File_Name_Upper'Last);
      in_file_id      : Ada.Streams.Stream_IO.File_Type;
      image_desc      : GID.Image_descriptor;
      Image_Format    : Unbounded_String;
      Width           : Positive;
      Height          : Positive;
      Orientation     : Unbounded_String;
      next_frame      : Ada.Calendar.Day_Duration := 0.0;
      Buffer_Index    : Natural := 0;
   begin
      Put_Line (Routine_Name & "processing " & Image_File_Name);
      --  Load the image in its original format
      Open (in_file_id, In_File, Image_File_Name);

      GID.Load_image_header
        (image_desc, Stream (in_file_id).all,
         try_tga =>
           Image_File_Name'Length >= 4 and then
         File_Kind = ".TGA");
      Image_Format := To_Unbounded_String (GID.Image_format_type'Image
                                           (GID.Format (image_desc)));
      Put_Line (Routine_Name & "image format: " & To_String (Image_Format));
      Orientation := To_Unbounded_String
        (GID.Orientation'Image (GID.Display_orientation (image_desc)));
      Put_Line (Routine_Name & "orientation: " & To_String (Orientation));
      Width := GID.Pixel_width (image_desc);
      Height := GID.Pixel_height (image_desc);
      Put_Line (Routine_Name & "Width, Height: " & Integer'Image (Width) &
                  Integer'Image (Height));

      Load_raw_image (image_desc, img_buf, next_frame);
      Close (in_file_id);

      Assert (next_frame = 0.0, Routine_Name & "animation is not supported ");

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

         return Image_Data;
      end;  --  declare block

   exception
      when GID.unknown_image_format =>
         Put_Line (Routine_Name & "image format is unknown!");
         if Is_Open (in_file_id) then
            Close (in_file_id);
         end if;
         raise;

      when Unsupported_Image_Format =>
         Put_Line (Routine_Name & "image format is not supported");
         if Is_Open (in_file_id) then
            Close (in_file_id);
         end if;
         raise;

   end Process;

   --  -------------------------------------------------------------------------

end JPEG_To_BMP;
