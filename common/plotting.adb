
--  with Ada.Text_IO; use Ada.Text_IO;

with PLplot_Auxiliary; use PLplot_Auxiliary;
with PLplot_Standard; use PLplot_Standard;

package body Plotting is

   procedure Set_Grey_Colourmap (Num_Colours : Integer) is
      R   : constant Real_Vector (0 .. 1) := (0.0, 1.0);
      G   : constant Real_Vector (0 .. 1) := (0.0, 1.0);
      B   : constant Real_Vector (0 .. 1) := (0.0, 1.0);
      Pos : constant Real_Vector (0 .. 1) := (0.0, 1.0);
   begin
      Set_Number_Of_Colors_In_Color_Map_1 (Num_Colours);
      Set_Color_Map_1_Piecewise (RGB, Pos, R, G, B, Alt_Hue_Path_None);
      
   end Set_Grey_Colourmap;
   
   --  -------------------------------------------------------------------------

   procedure Plot (Bitmap : ML_Types.Integer_List_2D) is
      Num_Rows    : constant Positive := Positive (Bitmap.Length);
      Num_Cols    : constant Positive := Positive (Bitmap.Element (1).Length);
      Num_Colours : constant Integer := 255;
      Image       : Real_Matrix (0 .. Num_Rows - 1, 0 .. Num_Cols - 1);
   begin
      Initialize_PLplot;
      Set_Grey_Colourmap (Num_Colours);

      Set_Environment (1.0, Long_Float (Num_Cols), 1.0, Long_Float (Num_Rows),
                       Justified, Box);
      --     Write_Labels ("", " ", "");
      Draw_Image_Color_Map_1_Automatic
        (Image, 1.0, Long_Float (Num_Cols), 1.0, Long_Float (Num_Rows),
         0.0, 0.0, 1.0, Long_Float (Num_Cols), 1.0, Long_Float (Num_Rows));

      End_PLplot;
   
   end Plot;
   
end Plotting;
