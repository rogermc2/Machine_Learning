
with
Ada.Text_IO,
     Ada.Sequential_IO,
     PLplot_Auxiliary,
     PLplot_Standard;
use
  Ada.Text_IO,
    PLplot_Auxiliary,
    PLplot_Standard;

procedure Example_20a is

   XDIM       : constant Integer := 260;
   YDIM       : constant Integer := 220;
   dbg        : constant Integer := 0;
   
   z          : Real_Matrix(0 .. XDIM - 1, 0 .. YDIM - 1);
   width, height, num_col : Integer;
   img_f      : Real_Matrix(0 .. 310, 0 .. 239); -- Chloe is width 311, height 240.
   
   type stretch_data is
      record
         xmin, xmax, ymin, ymax : Long_Float;
         stretch                : Long_Float;
      end record;

   -- Read image from file in binary ppm format.
   procedure read_img
     (fname                  : String; img_f  : out Real_Matrix;
      width, height, num_col : out Integer) is
      type Byte is mod 2 ** 8;
      A_Byte : Byte;
      package Chloe_IO is new Ada.Sequential_IO(Byte);
      use Chloe_IO;
      Input_File : Chloe_IO.File_Type;
   begin
      --  Chloe.pgm has 15 bytes of header followed by
      --  311 * 240 bytes of 8-bit pixels.
      Chloe_IO.Open (Input_File, In_File, fname);

      for i in 1 .. 15 loop
         Chloe_IO.Read(Input_File, A_Byte);
      end loop;

      width  := 311;  -- columns
      height := 240;  -- rows
      num_col := 255; -- number of colors

      for j in img_f'range(2) loop
         for i in img_f'range(1) loop
            Chloe_IO.Read(Input_File, A_Byte);
            img_f(i, height - j - 1) := Long_Float(A_Byte); -- Flip image up-down.
         end loop;
      end loop;
   end read_img;

   -- Set gray colormap.
   procedure gray_cmap (num_col : Integer) is
      r, g, b, pos : Real_Vector(0 .. 1);
   begin
      r(0) := 0.0;
      g(0) := 0.0;
      b(0) := 0.0;

      r(1) := 1.0;
      g(1) := 1.0;
      b(1) := 1.0;

      pos(0) := 0.0;
      pos(1) := 1.0;

      Set_Number_Of_Colors_In_Color_Map_1(num_col);
      Set_Color_Map_1_Piecewise(RGB, pos, r, g, b, Alt_Hue_Path_None);
   end gray_cmap;

begin
   Initialize_PLplot;

   -- View image border pixels.
   if dbg /= 0 then
      Set_Environment(1.0, Long_Float(XDIM), 1.0, Long_Float(YDIM), 1, 1); -- no plot box

      -- Build a one pixel square border, for diagnostics.
      for i in z'range(1) loop
         z(i, YDIM - 1) := 1.0; -- right
      end loop;

      for i in z'range(1) loop
         z(i, 0) := 1.0; -- left
      end loop;

      for i in z'range(2) loop
         z(0, i) := 1.0; -- top
      end loop;

      for i in z'range(2) loop
         z(XDIM - 1, i) := 1.0; -- botton
      end loop;

      Write_Labels("...around a blue square."," ","A red border should appear...");

      Draw_Image_Color_Map_1_Automatic(z,
                                       1.0, Long_Float(XDIM), 1.0, Long_Float(YDIM), 0.0, 0.0,
                                       1.0, Long_Float(XDIM), 1.0, Long_Float(YDIM));
   end if;

   -- Read the Chloe image.
   -- Note we try two different locations to cover the case where this
   -- examples is being run from the test_c.sh script.
   read_img("./Chloe.pgm", img_f, width, height, num_col);

   -- Set gray colormap.
   gray_cmap (num_col);

   -- Display Chloe.
   Set_Environment(1.0, Long_Float(width), 1.0, Long_Float(height), Justified, Box);
   Write_Labels (""," ","Chloe...");

   Draw_Image_Color_Map_1_Automatic (img_f, 1.0, Long_Float(width), 1.0, Long_Float(height), 0.0, 0.0, 1.0,
                                     Long_Float(width), 1.0, Long_Float(height));

   End_PLplot;
   
exception
   when NAME_ERROR =>
      Put_Line("Failed to open Chloe.pgm. Aborting.");
      End_PLplot;
      return;
   
end Example_20a;
