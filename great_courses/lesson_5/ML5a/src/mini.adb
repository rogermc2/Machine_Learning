--
--  Convert any image or animation file to PPM file(s).
--
--  Small-size demo for the GID (Generic Image Decoder) package.
--  For a larger example, look for to_bmp.adb .
--

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GID;

with Ada.Calendar;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with Interfaces;

procedure Mini is

   procedure Blurb is
   begin
      Put_Line ("Mini * Converts any image file to a PPM file");
      Put_Line ("Simple test for the GID (Generic Image Decoder) package");
      Put_Line ("Package version " & GID.version & " dated " & GID.reference);
      Put_Line ("URL: " & GID.web);
      New_Line;
      Put_Line ("Syntax:");
      Put_Line ("mini <image_1> [<image_2>...]");
      New_Line;
   end Blurb;

   use Interfaces;

   type Byte_Array is array (Integer range <>) of Unsigned_8;
   type p_Byte_Array is access Byte_Array;
   procedure Dispose is new Ada.Unchecked_Deallocation (Byte_Array, p_Byte_Array);

   img_buf : p_Byte_Array := null;

   --  Load image into a 24-bit truecolor RGB raw bitmap (for a PPM output)
   procedure Load_raw_image (image      : in out GID.Image_descriptor;
                             buffer     : in out p_Byte_Array;
                             next_frame : out Ada.Calendar.Day_Duration) is
      subtype Primary_color_range is Unsigned_8;
      image_width  : constant Positive := GID.Pixel_width (image);
      image_height : constant Positive := GID.Pixel_height (image);
      idx          : Natural;
      --
      procedure Set_X_Y (x, y : Natural) is
      begin
         idx := 3 * (x + image_width * (image_height - 1 - y));
      end Set_X_Y;
      --
      procedure Put_Pixel (red, green, blue : Primary_color_range;
                           alpha            : Primary_color_range) is
         pragma Warnings (off, alpha); -- alpha is just ignored
      begin
         buffer (idx .. idx + 2) := (red, green, blue);
         idx := idx + 3;
         --  ^ GID requires us to look to next pixel on the right for next time.
      end Put_Pixel;

      stars  : Natural := 0;
      procedure Feedback (percents : Natural) is
         so_far : constant Natural := percents / 5;
      begin
         for i in stars + 1 .. so_far loop
            Put (Standard_Error, '*');
         end loop;
         stars := so_far;
      end Feedback;

      procedure Load_image is
        new GID.Load_image_contents (Primary_color_range, Set_X_Y,
                                     Put_Pixel, Feedback, GID.fast);

   begin
      Dispose (buffer);
      buffer := new Byte_Array (0 .. 3 * image_width * image_height - 1);
      Load_image (image, next_frame);
   end Load_raw_image;

   procedure Dump_PPM (Name : String; i : GID.Image_descriptor) is
      use Ada.Strings.Fixed;
      Pos      : constant Natural := Index (name, ".");
      f        : Ada.Streams.Stream_IO.File_Type;
      Out_Name : Unbounded_String := To_Unbounded_String (Name);
   begin
      if Pos > 0 then
         Out_Name := To_Unbounded_String (Slice (Out_Name, 1, Pos - 1));
      end if;

      Create (f, Out_File, To_String (Out_Name) & ".ppm");
      --  PPM Header:
      String'Write (Stream (f),
                    "P6 " &
                      Integer'Image (GID.Pixel_width (i)) &
                      Integer'Image (GID.Pixel_height (i)) & " 255" & ASCII.LF);
      --  PPM raw BGR image:
      Byte_Array'Write (Stream (f), img_buf.all);
      --  ^ slow on some Ada systems, see to_bmp to have a faster version
      Close (f);
   end Dump_PPM;

   procedure Process (File_Name : String) is
      f             : Ada.Streams.Stream_IO.File_Type;
      i             : GID.Image_descriptor;
      up_name       : constant String := To_Upper (File_Name);
      next_frame,
      current_frame : Ada.Calendar.Day_Duration := 0.0;
   begin
      --  Load the image in its original format
      Open (f, In_File, File_Name);
      Put_Line (Standard_Error, "Processing " & File_Name & "...");

      GID.Load_image_header (i, Stream (f).all,
                             try_tga =>
                               File_Name'Length >= 4 and then
                             up_name (up_name'Last - 3 .. up_name'Last) = ".TGA");
      Put_Line (Standard_Error, ".........v.........v");

      loop
         Load_raw_image (i, img_buf, next_frame);
         Dump_PPM (File_Name & Duration'Image (current_frame), i);
         New_Line (Standard_Error);
         exit when next_frame = 0.0;
         current_frame := next_frame;
      end loop;
      Close (f);
   end Process;

begin
   if Argument_Count = 0 then
      Blurb;
      return;
   end if;
   for i in 1 .. Argument_Count loop
      Process (Argument (i));
   end loop;
end Mini;
