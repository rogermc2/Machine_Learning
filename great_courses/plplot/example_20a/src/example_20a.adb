
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Sequential_IO;

with PLplot_Auxiliary; use PLplot_Auxiliary;
with PLplot_Standard; use PLplot_Standard;

procedure Example_20a is
   
   type Byte is mod 2 ** 8;
   A_Byte : Byte;
   package Chloe_IO is new Ada.Sequential_IO (Byte);
   
   width  : constant Integer := 311;  -- columns
   height : constant Integer := 240;  -- rows
   num_col : constant Integer := 255; -- number of colors

   --  Chloe is width 311, height 240.
   img_f                  : Real_Matrix(0 .. 310, 0 .. 239);

   -- Read image from file in binary ppm format.
   procedure read_image
     (File_Name           : String; img_f  : out Real_Matrix) is
      use Chloe_IO;
      Input_File : Chloe_IO.File_Type;
   begin
      --  Chloe.pgm has 15 bytes of header followed by
      --  311 * 240 bytes of 8-bit pixels.
      Chloe_IO.Open (Input_File, In_File, File_Name);

      --  Skip header
      for i in 1 .. 15 loop
         Chloe_IO.Read (Input_File, A_Byte);
      end loop;

      for j in img_f'range (2) loop
         for i in img_f'range (1) loop
            Chloe_IO.Read (Input_File, A_Byte);
            --  Flip image up-down.
            img_f (i, height - j - 1) := Long_Float (A_Byte);
         end loop;
      end loop;
      
   end read_image;

   -- Set gray colormap.
   procedure gray_cmap (num_col : Integer) is
      r, g, b, pos : Real_Vector(0 .. 1);
   begin
      r (0) := 0.0;
      g (0) := 0.0;
      b (0) := 0.0;

      r (1) := 1.0;
      g (1) := 1.0;
      b (1) := 1.0;

      pos (0) := 0.0;
      pos (1) := 1.0;

      Set_Number_Of_Colors_In_Color_Map_1 (num_col);
      Set_Color_Map_1_Piecewise (RGB, pos, r, g, b, Alt_Hue_Path_None);
      
   end gray_cmap;

begin
   Initialize_PLplot;
   read_image ("./Chloe.pgm", img_f);

   --  Set gray colormap.
   gray_cmap (num_col);

   --  Display Chloe.
   Set_Environment (1.0, Long_Float (width), 1.0, Long_Float (height),
                    Justified, Box);
   Write_Labels ("", " ", "Chloe...");
   Draw_Image_Color_Map_1_Automatic
     (img_f, 1.0, Long_Float (width), 1.0, Long_Float (height), 0.0, 0.0, 1.0,
      Long_Float (width), 1.0, Long_Float (height));

   End_PLplot;
   
exception
   when NAME_ERROR =>
      Put_Line("Failed to open Chloe.pgm. Aborting.");
      End_PLplot;
      return;
   
end Example_20a;
