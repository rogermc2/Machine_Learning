
--  with Ada.Text_IO; use Ada.Text_IO;

with PLplot_Auxiliary; use PLplot_Auxiliary;
with PLplot_Standard; use PLplot_Standard;

package body Plotting is
   
   width  : constant Integer := 311;  -- columns
   height : constant Integer := 240;  -- rows
   num_col : constant Integer := 255; -- number of colors

   --  Set gray colormap.
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

   procedure Plot is
      image  : Real_Matrix(0 .. 310, 0 .. 239);
   begin
      Initialize_PLplot;
      --  Set gray colormap.
      gray_cmap (num_col);

      Set_Environment (1.0, Long_Float (width), 1.0, Long_Float (height),
                       Justified, Box);
      --     Write_Labels ("", " ", "");
      Draw_Image_Color_Map_1_Automatic
        (image, 1.0, Long_Float (width), 1.0, Long_Float (height), 0.0, 0.0, 1.0,
         Long_Float (width), 1.0, Long_Float (height));

      End_PLplot;
   
   end Plot;
   
end Plotting;
